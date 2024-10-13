#!/bin/sh

# The purpose of this script is to provide a small amount of information to help
# characterize failures that are difficult to reproduce outside the CI runner.
# It is not expected to be thorough.
#
# It would probably be more helpful for the test scripts to be resilient to
# failures in lilypond and to generate a difference report that allows us to
# browse the logs of all test cases that lack output because lilypond exited
# prematurely.

set -eu

LOGS=$(find out/lybook-testdb -name "*.log" | xargs grep -l "fatal error: ")

for log in $LOGS ; do
    ly=${log%%.log}.ly
    if [ ! -f "$ly" ] ; then
        continue
    fi
    ly=$(awk '/sourcefilename/ { gsub(/"/, "", $2); print $2 }' $ly)
    if [ -n "$ly" ] ; then
        echo "===== begin log of one test with a fatal error ====="
        cat $log
        echo "====== end log of one test with a fatal error ======"
        break
    fi
done
