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
rm -rf $outdir &&
mkdir $outdir &&

log "compiling main program" &&
ghc -O2 -i$which --make -outputdir $outdir -o $outdir/Main Main.hs &&

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

log "all pass, good work!"
