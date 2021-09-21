all: cabal

cabal: haskell-verified-examples.cabal

haskell-verified-examples.cabal: package.yaml
	hpack

ci: haskell-verified-examples.cabal
	nix-shell --run "cabal test"

ghcid: haskell-verified-examples.cabal
	ghcid --command "cabal repl tests" --test Main.main
