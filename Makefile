all: cabal

cabal: haskell-verified-examples.cabal

haskell-verified-examples.cabal: package.yaml
	hpack

ghcid: haskell-verified-examples.cabal
	ghcid --command "cabal repl tests" --test Main.main
