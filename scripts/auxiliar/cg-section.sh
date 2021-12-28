#!/bin/sh
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2010--2022 Carl D. Sorensen <c_sorensen@byu.edu>
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


#  Build html versions of sections of LilyPond Contributor's Guide
#
#  Usage:  ./cg-section.sh SECTION
#
#    where SECTION is the section to be built.
#
#  For example, CG 4 would be built by
#       ./cg-section.sh doc-work
#
#  At the end of the run, the user is prompted whether or not to
#  remove the generated files.
#
#  Paths are calculated from environment variables in exactly the
#  same way as they are for doc-section.sh - see the documentation
#  for that.
#
#  Known limitations:
#
#     * Doesn't use website css files
#     * Bitmap images aren't loaded properly
#

usage () {
    cat <<EOF >&2
Usage: $0 SECTION

e.g. $0 doc-work
EOF
    exit "$1"
}

if [ "$1" = '-h' ] || [ "$1" = '--help' ]; then
    usage 0
fi

[ $# = 1 ] || usage 1

if [ -n "$LILYPOND_GIT" ]; then
    echo "Using source tree from value of \$LILYPOND_GIT: $LILYPOND_GIT"
else
    cd "`dirname $0`"
    cd ../..
    LILYPOND_GIT="`pwd`"
    echo "\$LILYPOND_GIT was not set; auto-detected source tree at $LILYPOND_GIT"
fi

if test ! -e "$LILYPOND_GIT/DEDICATION"; then
    echo "Error: $LILYPOND_GIT did not look like a LilyPond source tree; aborting." >&2
    exit 1
fi

: "${LILYPOND_BUILD_DIR:=$LILYPOND_GIT/build}"
DOC_DIR="${LILYPOND_TEMPDOCS:-$LILYPOND_BUILD_DIR/tempdocs}"
LILYPOND_BOOK="$LILYPOND_BUILD_DIR/out/bin/lilypond-book"
TEXI2HTML="texi2html"
REFCHECK="$LILYPOND_GIT/scripts/auxiliar/ref_check.py"

SECTION="$1"
OUTPUT_DIR="$DOC_DIR/contributor"
SECTION_PATH="$LILYPOND_GIT/Documentation/en/contributor/$SECTION.itexi"

if test ! -e "$SECTION_PATH"; then
    echo "$SECTION_PATH did not exist; is $SECTION a valid section in the Contributor's Guide?" >&2
    exit 1
fi

if test ! -d "$OUTPUT_DIR/out"; then
    mkdir -p "$OUTPUT_DIR/out"
fi

cp "$LILYPOND_GIT/Documentation/en/common-macros.itexi" "$OUTPUT_DIR/common-macros.itexi"
cp "$LILYPOND_GIT/Documentation/en/macros.itexi" "$DOC_DIR/macros.itexi"
cp "$LILYPOND_BUILD_DIR/Documentation/out/version.itexi" "$OUTPUT_DIR"

if test -e "$OUTPUT_DIR/$SECTION.html"; then
    rm "$OUTPUT_DIR/$SECTION.html"
fi

if test -e "$OUTPUT_DIR/out/$SECTION.texi"; then
    rm "$OUTPUT_DIR/out/$SECTION.texi"
fi

echo "Running RefCheck"
python3 "$REFCHECK"

cd "$DOC_DIR"
echo "Running $TEXI2HTML"
cat "$DOC_DIR/macros.itexi" "$SECTION_PATH" > "$OUTPUT_DIR/$SECTION.texi"

"$TEXI2HTML" \
    --no-validate \
    --output="$OUTPUT_DIR/out/$SECTION.html" \
    --I="$LILYPOND_GIT/Documentation" \
    --I="$OUTPUT_DIR/out" \
    "$OUTPUT_DIR/$SECTION.texi"

cat <<EOF

The $SECTION section of the Contributor's Guide should now be viewable at

  $OUTPUT_DIR/out/$SECTION.html

If you want to keep the generated docs around for a while, answer
'n' to the next question.  If you only needed them to quickly check
something, view them now and then answer 'y' when you're done.

EOF

read -p "rm -rf $OUTPUT_DIR ? (y/n): "
if [ "$REPLY" = "y" ]; then
    echo "deleting files"
    rm -rf "$OUTPUT_DIR"
fi
