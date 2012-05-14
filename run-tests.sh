#!/bin/sh

which="$1"

if [ "" = "$1" ]; then
    which="versions/initial/"
fi

outdir="dist"

echo ">> cleaning" &&
rm -rf $outdir &&
mkdir $outdir &&

echo ">> compiling main program" &&
ghc -i$which --make -outputdir $outdir -o $outdir/Main Main.hs &&

echo ">> compiling unit test program" &&
ghc -i$which --make -outputdir $outdir -o $outdir/UnitTests $which/UnitTests.hs &&

echo ">> compiling acceptance test program" &&
ghc --make -outputdir $outdir -o $outdir/AcceptanceTests AcceptanceTests.hs &&

echo ">> running unit tests" &&
./dist/UnitTests &&

echo ">> running acceptance tests" &&
./dist/AcceptanceTests &&

echo ">> running performace test" &&
time echo "abcdefghijklmnopq" | ./dist/Main test_programs/echo_until_q.bf &&

echo ">> all pass, good work!"
