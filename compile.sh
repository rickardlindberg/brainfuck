#!/bin/sh

outdir="dist"

mkdir -p $outdir

set -x

ghc --make -outputdir $outdir -o $outdir/Main Main.hs &&
ghc --make -outputdir $outdir -o $outdir/UnitTests UnitTests.hs &&
ghc --make -outputdir $outdir -o $outdir/AcceptanceTests AcceptanceTests.hs
