test: clean configure-test build run-test

clean:
	cabal-dev clean

configure:
	cabal-dev configure

configure-test:
	cabal-dev configure --flags=test

build:
	cabal-dev build

run-test:
	dist/build/test_runner/test_runner
