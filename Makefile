CABALSANDBOX := ".cabal-sandbox"

.PHONY: bench clean haddock hpc init install repl run test

all: install hpc bench haddock run

bench: install
	cabal configure --enable-benchmarks
	cabal build
	cabal bench --benchmark-options="-o report.html"

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

haddock:
	cabal configure
	cabal build
	cabal haddock --hyperlink-source
	# dist/doc/html/threase/index.html

hpc: test
	hpc report dist/hpc/tix/hspec/hspec.tix
	hpc markup --destdir=tmp dist/hpc/tix/hspec/hspec.tix
init:
	cabal update
	cabal sandbox init

install: init
	cabal install --enable-benchmarks --enable-tests --flags=documentation --only-dependencies

repl:
	cabal configure
	cabal build
	cabal repl

test: install
	cabal configure --enable-tests
	cabal build
	cabal test
