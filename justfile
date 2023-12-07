default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ARGS}}

# Autoformat the project tree
fmt:
    treefmt

# Run ghcid -- auto-recompile and run `main` function
ghcid:
    ghcid -c "cabal repl exe:rescript-non-interference" --warnings -T :main

build:
    cabal build

run *ARGS: build
    cabal run rescript-non-interference {{ARGS}}

# Run ghcid -- auto-recompile and run `main` function
test:
    ghcid -c "cabal repl exe:rescript-non-interference-test" --warnings -T :main
