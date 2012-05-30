#!/bin/sh

which="$1"

if [ "" = "$1" ]; then
    which="versions/initial/"
fi

outdir="dist"

function log() {
    echo -e "\033[1m>> $1\033[0m"
}

log "cleaning" &&
rm -rf $outdir MainProfiling.prof &&
mkdir $outdir &&

log "compiling main program" &&
ghc -O2 -i$which --make -outputdir $outdir -o $outdir/Main Main.hs &&

log "compiling main program with profiling support" &&
profiling="-rtsopts -prof -auto-all -caf-all -fforce-recomp" &&
ghc $profiling -O2 -i$which --make -outputdir $outdir -o $outdir/MainProfiling Main.hs &&

log "compiling unit test program" &&
ghc -O2 -i$which --make -outputdir $outdir -o $outdir/UnitTests $which/UnitTests.hs &&

log "compiling acceptance test program" &&
ghc -O2 --make -outputdir $outdir -o $outdir/AcceptanceTests AcceptanceTests.hs &&

log "running unit tests" &&
./dist/UnitTests &&

log "running acceptance tests" &&
./dist/AcceptanceTests &&

log "running performace test" &&
time echo "abcdefghijklmnopq" | ./dist/Main test_programs/echo_until_q.bf &&

log "comparing to c version" &&
gcc versions/c/BFI.c -o dist/BFI &&
time echo "abcdefghijklmnopq" | ./dist/BFI test_programs/echo_until_q.bf &&

log "running profiling test" &&
echo "abcdefghijklmnopq" | ./dist/MainProfiling test_programs/echo_until_q.bf +RTS -sstderr -p &&

log "all pass, good work!"
