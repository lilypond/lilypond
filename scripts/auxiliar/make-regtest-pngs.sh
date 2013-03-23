#!/bin/sh

#  Make PNG files from regtests
#
#  Usage:  ./make-regtest-pngs.sh -j CPUs -o/-n
#
#    where -j specifies the number of parallel processes to run
#    (normally CPUs+1).  e.g.:
#
#    ./make-regtest-pngs.sh -j9
#
#   -o means build an old regtest set - the PNGs go in the old-regtest-results
#   directory
#
#   -n means build a new regtest set - the PNGs go in the new-regtest-results
#   directory

cpu_count=1

while getopts "j:on" opts; do
    if [ "$opts" = "j" ]; then
        cpu_count=$OPTARG
    fi

    if [ "$opts" = "o" ]; then
        file_loc="old-regtest-results"
    fi

    if [ "$opts" = "n" ]; then
        file_loc="new-regtest-results"
        do_compare="y"
    fi
done

if [ -z "$file_loc" ]; then
    echo "Must specify old (-o) or new (-n) regtest PNG creation on command line"
    exit 1
fi

rm -rf $LILYPOND_BUILD_DIR/out-png-check/$file_loc
mkdir -p $LILYPOND_BUILD_DIR/out-png-check/$file_loc
cd $LILYPOND_BUILD_DIR/out-png-check/$file_loc
ls $LILYPOND_GIT/input/regression/*.ly > dir.txt
$LILYPOND_BUILD_DIR/out/bin/lilypond --png --relocate \
    -dinclude-settings=$LILYPOND_GIT/scripts/auxiliar/NoTagline.ly \
    -djob-count=$cpu_count -dread-file-list "dir.txt"
rm -rf dir.txt
rm -rf *.log

if [ -n "$do_compare" ]; then
    cd ..
    rm -rf regtest-diffs
    mkdir -p regtest-diffs
    diff_count=0
    for filename in new-regtest-results/*.png; do
        trimFile=$(basename $filename)
        if [ -e old-regtest-results/$trimFile ]; then
            convert new-regtest-results/$trimFile -level 50%  NewTest.png
            convert old-regtest-results/$trimFile -level 50%  OldTest.png
            difference=$(compare -metric AE NewTest.png OldTest.png null: 2>&1 )
            if [ $? -gt 0 ];then
                difference=9999
            fi
            if [ $difference -gt 1 ];then
                echo $trimFile": "$difference" differences"
                compare -dissimilarity-threshold 1 \
                    new-regtest-results/$trimFile \
                    old-regtest-results/$trimFile  regtest-diffs/$trimFile
                convert regtest-diffs/$trimFile -trim regtest-diffs/$trimFile
                diff_count=$(($diff_count+1))
            fi
        else
            echo "old-regtest-results/"$trimFile" does not exist"
        fi
    done
    rm -rf NewTest.png
    rm -rf OldTest.png
    echo $diff_count "differences found."
fi
