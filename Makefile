all: cabal

cabal: haskell-verify-examples.cabal

haskell-verify-examples.cabal: package.yaml
	hpack

check: haskell-verify-examples.cabal
	cabal test

install-exe: clean haskell-verify-examples.cabal
	cabal install exe:haskell-verify-examples --overwrite-policy=always

clean:
	rm -rf ./dist-newstyle
	rm -rf *.cabal
	rm -rf ~/haskell-verify-examples.overmind.sock

watch: clean haskell-verify-examples.cabal
	OVERMIND_CAN_DIE=cli-smoke-test \
	SHELL=`which bash` \
	overmind start --title haskell-verify-examples \
	--procfile Procfile \
	--socket ~/haskell-verify-examples.overmind.sock \
	--tmux-config /dev/null
