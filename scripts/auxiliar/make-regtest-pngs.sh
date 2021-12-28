#!/bin/sh
#
#  Make PNG files from regtests

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2013--2022 Phil Holmes <mail@philholmes.net>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


#  Usage:  ./make-regtest-pngs.sh -j CPUs -o/-n
#
#    where -j specifies the number of parallel processes to run
#    (normally CPUs+1).  e.g.:
#
#    ./make-regtest-pngs.sh -j9
#
#   -p uses GNU parallel with the given job count in order to also
#   parallelize the conversion of PDF files to bitmaps when using -g
#   or -d.  No attempt is made to parallelize the bitmap comparisons
#   since their memory requirements may be prohibitive.
#
#   -o means build an old regtest set - the PNGs go in the old-regtest-results
#   directory
#
#   -n means build a new regtest set - the PNGs go in the new-regtest-results
#   directory
#
#   -c uses PDF and the poppler library via pdftocairo for generating
#   bitmaps, simulating the output for Evince and other previewers
#   using poppler.  pdftocairo may be contained in the poppler-utils
#   package.
#
#   -r can be used for specifying a rendering resolution.  This
#   defaults to 101 for poppler and 300 for Ghostscript from PDF.
#
#   -g uses Ghostscript for rendering a bitmap version from the PDF,
#   simulating the output from printing PDF files on a GNU system, so
#   use a resolution appropriate for print.  Antialiasing is not enabled.
#
#   -d changes the Ghostscript device used for creating PNG files
#   (usually png16m for direct PNG creation and pngmono for printer simulation)
#
#   if any filenames follow, those are the tests to run.  In absence
#   of any filenames, the contents of input/regression are used.

cpu_count=${CPU_COUNT:-1}
backend_opt='--png ${resolution:+=-dresolution=$resolution} ${gsdevice:+=-dpixmap-format=$gsdevice}'
resolution=
gsdevice=
use_parallel=n

while getopts "j:oncr:gpd:" opts; do
    case $opts in j)
            cpu_count=$OPTARG;;
	
	o)
            file_loc="old-regtest-results";;

	c)
	    backend_opt="--pdf"
	    png_generate()
	    {
		$1 pdftocairo -png -r ${resolution:-101} -q "$2"
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
		$1 gs -sDEVICE=${gsdevice:-pngmono} \
                      -q -dNOPAUSE \
                      -r${resolution:-300} \
                      -dNOPLATFONTS \
                      -dAutoRotatePages=/None \
                      -dPrinted=false \
                      -dTextAlphaBits=1 \
                      -dGraphicsAlphaBits=1 \
                      -sOutputFile="${2%.pdf}-%d.png" \
                      "$2" \
                      -c quit
	    };;
	p)
	    use_parallel=y;;
	d)
	    gsdevice=$OPTARG;;
    esac
done

shift $((OPTIND-1))

if [ -z "$file_loc" ]; then
    echo "Must specify old (-o) or new (-n) regtest PNG creation on command line"
    exit 1
fi

rm -rf $LILYPOND_BUILD_DIR/out-png-check/$file_loc
mkdir -p $LILYPOND_BUILD_DIR/out-png-check/$file_loc
OLDPWD="$PWD"
cd $LILYPOND_BUILD_DIR/out-png-check/$file_loc
if [ "$*" = "" ]
then
    ls $LILYPOND_GIT/input/regression/*.ly > dir.txt
else
    : > dir.txt
    for i
    do
	case "$i" in /*)
		echo "$i" >> dir.txt;;
	    *)
		echo "$OLDPWD/$i" >> dir.txt
	esac
    done
fi

$LILYPOND_BUILD_DIR/out/bin/lilypond $(eval echo $backend_opt) --relocate \
    -dinclude-settings=$LILYPOND_GIT/scripts/auxiliar/NoTagline.ly \
    -djob-count=$cpu_count -dread-file-list "dir.txt"
if [ "$backend_opt" = "--pdf" ]
then
    if [ $use_parallel = y ]
    then
	for i in *.pdf
	do
	    png_generate echo $i
	done | parallel -j $cpu_count
	rm *.pdf
    else
	for i in *.pdf
	do
	    png_generate "" $i && rm "$i"
	done
    fi
fi
rm -rf dir.txt
rm -rf *.log

if [ -n "$do_compare" ]; then
    cd ..
    rm -rf regtest-diffs
    mkdir -p regtest-diffs
    diff_count=0
    for filename in new-regtest-results/*.png; do
        trimFile=$(basename $filename)
        if [ -e "old-regtest-results/$trimFile" ]; then
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
