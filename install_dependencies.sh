#! /bin/bash
cabal sandbox init
cabal install --dependencies-only --allow-newer=transformers --enable-tests
