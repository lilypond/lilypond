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
#
#   -p uses PDF and the poppler library via pdftocairo for generating bitmaps,
#   simulating the output for Evince and other previewers using poppler.
#   pdftocairo may be contained in the poppler-utils package.
#
#   -r can be used for specifying a rendering resolution.  This
#   defaults to 101 for poppler and 300 for Ghostscript from PDF.
#
#   -g uses Ghostscript for rendering a bitmap version from the PDF,
#   simulating the output from printing PDF files on a GNU system, so
#   use a resolution appropriate for print.  Antialiasing is not enabled.
#
#   -d changes the Ghostscript device used for creating PNG files
#   (usually png16m, but with -g you might prefer fewer colors for size
#   reasons, like png16).

cpu_count=${CPU_COUNT:-1}
backend_opt='--png ${resolution:+=-dresolution=$resolution} ${gsdevice:+=-dpixmap-format=$gsdevice}'
resolution=
gsdevice=

png_generate()
{
    :
}

while getopts "j:onpr:g" opts; do
    case $opts in j)
            cpu_count=$OPTARG;;
	
	o)
            file_loc="old-regtest-results";;

	p)
	    backend_opt="--pdf"
	    png_generate()
	    {
		for i
		do pdftocairo -png -r ${resolution:-101} -q "$i" &&
		    rm "$i"
		done
	    };;

	n)
            file_loc="new-regtest-results"
            do_compare="y";;

	r)
	    resolution=$OPTARG;;
	g)
	    backend_opt="--pdf"
	    png_generate()
	    {
		for i
		do
		    gs -sDEVICE=${gsdevice:-png16m} -q -dNOPAUSE \
			-r${resolution:-300} -dNOPLATFONTS \
			-dTextAlphaBits=1 -dGraphicsAlphaBits=1 \
			-sOutputFile="${i%.pdf}-%d.png" "$i" -c quit &&
		    rm "$i"
		done
	    };;
    esac
done

if [ -z "$file_loc" ]; then
    echo "Must specify old (-o) or new (-n) regtest PNG creation on command line"
    exit 1
fi

rm -rf $LILYPOND_BUILD_DIR/out-png-check/$file_loc
mkdir -p $LILYPOND_BUILD_DIR/out-png-check/$file_loc
cd $LILYPOND_BUILD_DIR/out-png-check/$file_loc
ls $LILYPOND_GIT/input/regression/*.ly > dir.txt
$LILYPOND_BUILD_DIR/out/bin/lilypond $(eval echo $backend_opt) --relocate \
    -dinclude-settings=$LILYPOND_GIT/scripts/auxiliar/NoTagline.ly \
    -djob-count=$cpu_count -dread-file-list "dir.txt"
png_generate *.pdf
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
