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


#  Build html versions of sections of lilypond documentation
#
#  Usage:  ./doc-section.sh MANUAL SECTION
#
#    where MANUAL is the manual and SECTION is the section to be
#    built.
#
#  For example, NR 1.2 would be built by
#       ./doc-section.sh notation rhythms
#
#     and LM 1 would be built by
#       ./doc-section.sh learning tutorial
#
#  At the end of the run, the user is prompted whether or not to
#  remove the generated files.
#
#  According to https://sourceforge.net/p/testlilyissues/issues/1236/
#  the location of the lilypond git tree is taken from $LILYPOND_GIT
#  if specified, otherwise it is auto-detected.
#
#  It is assumed that compilation takes place in the build/
#  subdirectory, but this can be overridden by setting the environment
#  variable LILYPOND_BUILD_DIR.
#
#  Similarly, output defaults to build/tempdocs/ but this can be
#  overridden by setting the environment variable LILYPOND_TEMPDOCS.
#
#
#  Known limitations:
#
#     * Doesn't use website css files
#     * Bitmap images aren't loaded properly
#     * Won't build Contributors' Guide; see scripts/auxiliar/cg-section.sh
#

usage () {
    cat <<EOF >&2

Usage: $0 MANUAL SECTION
e.g. $0 notation rhythms

EOF
    exit "$1"
}

if [ "$1" = '-h' ] || [ "$1" = '--help' ]; then
    usage 0
fi

[ $# = 2 ] || usage 1

if [ -n "$LILYPOND_GIT" ]; then
    echo "Using source tree from value of \$LILYPOND_GIT: $LILYPOND_GIT"
else
    cd "`dirname $0`"
    cd ../..
    LILYPOND_GIT="`pwd`"
    echo "\$LILYPOND_GIT was not set; auto-detected source tree at $LILYPOND_GIT"
fi

if [ -n "$BROWSER" ]; then
    echo "Using browser from \$BROWSER: $BROWSER"
else
    echo "\$BROWSER not set; using firefox as default"
    BROWSER="firefox"
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

MANUAL="$1"
SECTION="$2"
OUTPUT_DIR="$DOC_DIR/$SECTION"
MANUAL_PATH="$LILYPOND_GIT/Documentation/en/$MANUAL"
SECTION_PATH="$MANUAL_PATH/$SECTION.itely"

if test ! -d "$LILYPOND_BUILD_DIR"; then
    echo "$LILYPOND_BUILD_DIR did not exist; check your setting of LILYPOND_BUILD_DIR. Aborting." >&2
    exit 1
fi

if test ! -d "$MANUAL_PATH"; then
    echo "$MANUAL_PATH was not a valid directory; is $MANUAL a valid manual?" >&2
    exit 1
fi

if test ! -e "$SECTION_PATH"; then
    echo "$SECTION_PATH did not exist; is $SECTION a valid section in the $MANUAL manual?" >&2
    exit 1
fi

if test ! -d "$DOC_DIR"; then
    mkdir -p "$DOC_DIR"
    cp "$LILYPOND_BUILD_DIR/Documentation/out/version.itexi" "$DOC_DIR"
fi
if test ! -d "$OUTPUT_DIR/out"; then
    mkdir -p "$OUTPUT_DIR/out"
fi

cp "$LILYPOND_GIT/Documentation/en/common-macros.itexi" "$OUTPUT_DIR/common-macros.itexi"
cp "$LILYPOND_GIT/Documentation/en/macros.itexi" "$DOC_DIR/macros.itexi"
cp "$DOC_DIR/version.itexi" "$OUTPUT_DIR/version.itexi"
cp -r "$LILYPOND_GIT/Documentation/pictures/" "$OUTPUT_DIR/out/pictures"

if test -e "$OUTPUT_DIR/$SECTION.html"; then
    rm "$OUTPUT_DIR/$SECTION.html"
fi

if test -e "$OUTPUT_DIR/out/$SECTION.texi"; then
    rm "$OUTPUT_DIR/out/$SECTION.texi"
fi

echo "Running $LILYPOND_BOOK"
"$LILYPOND_BOOK" \
        -f texi-html \
        -I "$LILYPOND_GIT/Documentation/snippets" \
        -I "$LILYPOND_GIT/Documentation/snippets/new" \
        -I "$LILYPOND_GIT/input/manual" \
        -I "$LILYPOND_GIT/Documentation" \
        -I "$LILYPOND_GIT/Documentation/included"  \
        -I "$LILYPOND_GIT/Documentation/pictures" \
        -o "$OUTPUT_DIR/out" \
        "$SECTION_PATH"
BOOKRC=$?
if [ "$BOOKRC" != 0 ]; then
    echo "Lilypond-book returned code $BOOKRC"
    exit $BOOKRC
fi

echo "Running RefCheck"
python3 "$REFCHECK"

cd "$DOC_DIR"
if test -f "$OUTPUT_DIR/out/$SECTION.texi"; then
    echo "Running $TEXI2HTML"
    cat "$DOC_DIR/macros.itexi" "$OUTPUT_DIR/out/$SECTION.texi" > "$OUTPUT_DIR/$SECTION.texi"
    "$TEXI2HTML" \
        --no-validate \
        --output="$OUTPUT_DIR/out/$SECTION.html" \
        --I="$OUTPUT_DIR/out" \
        "$OUTPUT_DIR/$SECTION.texi"
fi

echo "Displaying output in $BROWSER; close browser window when done."

$BROWSER $OUTPUT_DIR/out/$SECTION.html

cat <<EOF

If you want to avoid recompiling the snippets on the next
invocation with '$MANUAL $SECTION', answer 'n' to the next question.

EOF

echo "Delete temp files? [y/n]"
read REPLY;
if [ "$REPLY" = "y" ]; then
    echo "deleting files"
    rm -rf "$OUTPUT_DIR"
fi
