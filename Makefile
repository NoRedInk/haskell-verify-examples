all: cabal

cabal: haskell-verified-examples.cabal

haskell-verified-examples.cabal: package.yaml
	hpack

check: haskell-verified-examples.cabal
	cabal test

build-dir:
	mkdir -p .build

watch: build-dir haskell-verified-examples.cabal
	OVERMIND_CAN_DIE=cli-smoke-test \
	SHELL=`which bash` \
	overmind start --title haskell-verify-examples \
	--procfile Procfile \
	--socket ~/haskell-verify-examples.overmind.sock \
	--tmux-config /dev/null
