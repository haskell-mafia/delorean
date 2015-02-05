MFLAGS =
MAKEFLAGS = $(MFLAGS)
SANDBOX = .cabal-sandbox
CABAL_FLAGS =
DEPS = .cabal-sandbox/.cairn
P = lib/p/p.cabal

.PHONY: build test repl repl-test quick tags

default: repl

${P}:
	git submodule init
	git submodule update

${SANDBOX}: ${P}
	cabal sandbox init
	cabal sandbox add-source lib/p

${DEPS}: ${SANDBOX} $(wildcard *.cabal)
	cabal install -j --only-dependencies --enable-tests
	cabal configure --enable-tests ${CABAL_FLAGS}
	touch $@

build: ${DEPS}
	cabal build

test: ${DEPS}
	cabal test --log=/dev/stdout

repl: ${DEPS}
	cabal repl

repl-test: ${DEPS}
	cabal repl test

quick: ${DEPS}
	ghci -package-db=$(wildcard ${SANDBOX}/*-packages.conf.d) -isrc -itest test/test.hs

tags:
	hasktags -e src test
