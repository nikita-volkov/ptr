#!/bin/bash
set -eo pipefail

function format {
  for path in $(git diff --staged --name-only -- '*.cabal') $(git ls-files -om --exclude-standard -- '*.cabal'); do if test -f $path; then cabal-fmt --no-tabular -c $path 2> /dev/null || cabal-fmt --no-tabular -i $path; fi; done
  for path in $(git diff --staged --name-only -- '*.hs') $(git ls-files -om --exclude-standard -- '*.hs'); do if test -f $path; then ormolu -ic $path; fi; done
}

function gen_hie {
  gen-hie > hie.yaml
}

format
# gen_hie

# cabal update

cabal build all --enable-tests --enable-benchmarks -j +RTS -A128m -n2m -N -RTS --ghc-options="-Werror -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wredundant-constraints -Wunused-packages -Wno-name-shadowing -Wno-unused-matches -Wno-unused-do-bind -Wno-type-defaults"
# cabal build all --enable-tests --enable-benchmarks -j +RTS -A128m -n2m -N -RTS
# cabal test --test-show-details=direct --test-options="--qc-max-success=10000 --seed 1299275230 --match /TheatreDev/StmBased/allOf/"
# cabal test --test-show-details=direct --test-options="--expert --failure-report=.last-hspec-report --rerun --rerun-all-on-success"

# cabal haddock
