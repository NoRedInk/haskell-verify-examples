test: ghcid --command "cabal repl tests" --test Main.main --allow-eval
cli-smoke-test: ghcid --command "cabal repl exe:haskell-verify-examples" --test Main.main --setup ":set args ./test/assets/Simple.hs"
cli-docs-smoke-test: ghcid --command "cabal repl haskell-verify-examples:generate-documentation" --test Main.main --setup ":set args ./README.md"
