#!/bin/bash

set -x

# Install GHC and build dependencies
travis-wait-enhanced --interval=1m --timeout=40m -- cabal new-build --enable-tests --enable-benchmarks --only-dependencies
ret=$?
case "$ret" in
  0)
    # continue
    ;;
  124)
    echo "Timed out while installing dependencies."
    echo "Try building again by pushing a new commit."
    exit 1
    ;;
  *)
    echo "Failed to install dependencies; stack exited with $ret"
    exit "$ret"
    ;;
esac

# Build your project
cabal new-build --enable-tests --enable-benchmarks
cabal new-test --enable-tests
#cabal new-haddock
cabal check
cabal v2-sdist   # tests that a source-distribution can be generated
