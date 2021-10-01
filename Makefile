all: cabal

cabal: haskell-verify-examples.cabal

haskell-verify-examples.cabal: package.yaml
	hpack

check: haskell-verify-examples.cabal
	cabal test

watch: haskell-verify-examples.cabal
	OVERMIND_CAN_DIE=cli-smoke-test \
	SHELL=`which bash` \
	overmind start --title haskell-verify-examples \
	--procfile Procfile \
	--socket ~/haskell-verify-examples.overmind.sock \
	--tmux-config /dev/null
