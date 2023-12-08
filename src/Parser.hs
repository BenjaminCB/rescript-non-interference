module Parser where

import AST
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as Token

languageDef :: Token.LanguageDef ()
languageDef =
    emptyDef
        { Token.identStart = letter
        , Token.identLetter = alphaNum
        , Token.reservedNames =
            [ "let"
            , "ref"
            , "if"
            , "else"
            , "true"
            , "false"
            , "H"
            , "L"
            , "while"
            , "for"
            , "in"
            , "to"
            ]
        , Token.commentLine = "--"
        }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

integer :: Parser Integer
integer = Token.integer lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

semi :: Parser String
semi = Token.semi lexer

parseInput :: String -> Either ParseError Expr
parseInput = parse (whiteSpace >> seqExpressions) ""

seqExpressions :: Parser Expr
seqExpressions = do
    exprs <- expression `sepBy1` semi
    return $ foldr1 Seq exprs

-- Parser for expressions
expression :: Parser Expr
expression =
    whiteSpace
        *> ( try letExpr
                <|> try assign
                <|> try ifThenElse
                <|> try while
                <|> try for
                <|> try abstraction
                <|> try application
                <|> try binaryOperation
                <|> try reference
                <|> try dereference
                <|> try number
                <|> try boolean
                <|> variable
           )

number :: Parser Expr
number = N . fromIntegral <$> integer

boolean :: Parser Expr
boolean =
    do
        reserved "true"
        return $ B True
        <|> do
            reserved "false"
            return $ B False

-- Parser for references
reference :: Parser Expr
reference = do
    reserved "ref"
    expr <- parens expression
    return $ Ref expr

dereference :: Parser Expr
dereference = do
    _ <- string "!"
    Deref . V <$> identifier

-- Parser for let expressions
letExpr :: Parser Expr
letExpr = try explicit <|> infer
    where
        explicit = do
            reserved "let"
            name <- identifier
            _ <- char ':'
            level <- LH High <$ char 'H' <|> LH Low <$ char 'L'
            _ <- spaces *> char '=' <* spaces
            Let (V name) level <$> expression
        infer = do
            reserved "let"
            name <- identifier
            _ <- spaces *> char '=' <* spaces
            LetInf (V name) <$> expression

variable :: Parser Expr
variable = Var . V <$> identifier

ifThenElse :: Parser Expr
ifThenElse = do
    reserved "if"
    cond <- parens expression
    thenExpr <- braces seqExpressions
    reserved "else"
    elseExpr <- braces seqExpressions
    return $ IfThenElse cond thenExpr elseExpr

while :: Parser Expr
while = do
    reserved "while"
    cond <- parens expression
    body <- braces seqExpressions
    return $ While cond body

for :: Parser Expr
for = do
    reserved "for"
    name <- identifier
    _ <- spaces *> string "in" <* spaces
    start <- expression
    _ <- spaces *> string "to" <* spaces
    end <- expression
    body <- braces seqExpressions
    return $ For (V name) start end body

assign :: Parser Expr
assign = do
    name <- identifier
    _ <- string ":="
    Assign (V name) <$> expression

abstraction :: Parser Expr
abstraction = do
    _ <- char '('
    name <- identifier
    _ <- char ':'
    level <- LH High <$ char 'H' <|> LH Low <$ char 'L'
    _ <- char ')'
    _ <- spaces *> string "=>" <* spaces
    body <- braces seqExpressions
    return $ Abs (V name) level body

application :: Parser Expr
application = do
    func <- parens expression <|> variable
    arg <- parens expression
    return $ App func arg

binaryOperation :: Parser Expr
binaryOperation = do
    op <- Add <$ char '+' <|> Sub <$ char '-' <|> Mul <$ char '*' <|> Div <$ char '/'
    spaces
    left <- expression
    spaces
    BO op left <$> expression
