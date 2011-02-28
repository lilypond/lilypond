#!/bin/sh

#  Build html versions of sections of lilypond documentation
#
#  Usage:  doc-section.sh MANUAL SECTION
#
#   where MANUAL is the manual and SECTION is the section to be built.
#
#   For example, NR 1.2 would be built by
#       doc-section.sh notation rhythms
#
#     and LM 1 would be built by
#       doc-section.sh learning tutorial
#
#   At the end of the run, the user is prompted whether or not to remove files
#
#  Before first use, the following must be done:
#     * Set FROMDIR, DOCDIR, TODIR, LILYPONDBOOK, and TEXI2HTML for your system
#     * Create $DOCDIR
#     * Copy version.itexi from somewhere in your Documentation tree
#         (probably Documentation/out) to $DOCDIR
#
#  Known limitations:
#
#     * Doesn't use website css files
#     * Bitmap images aren't loaded properly
#     * Won't build Contributors' Guide; see scripts/auxiliar/cg-section.sh
#

#
#  Customize the file here
#
FROMDIR="$HOME/lilypond-git"
DOCDIR="$HOME/lilypond-git/tempdocs"
LILYPONDBOOK="lilypond-book"
TEXI2HTML="texi2html"
REFCHECK="$FROMDIR/scripts/auxiliar/ref_check.py"

DIRECTORY=$1
NAME=$2
TODIR=$DOCDIR/$NAME

if test ! -d $TODIR; then
  mkdir $TODIR
fi
if test ! -d $TODIR/out; then
  mkdir $TODIR/out
fi

cp $FROMDIR/Documentation/common-macros.itexi $TODIR/common-macros.itexi
cp $FROMDIR/Documentation/macros.itexi $DOCDIR/macros.itexi
cp $DOCDIR/version.itexi $TODIR/version.itexi

if test -e $TODIR/$NAME.html; then
  rm $TODIR/$NAME.html
fi

if test -e $TODIR/out/$NAME.texi; then
  rm $TODIR/out/$NAME.texi
fi

echo "Running lilypond-book"
$LILYPONDBOOK \
        -f texi-html \
        -I $FROMDIR/Documentation/snippets \
        -I $FROMDIR/Documentation/snippets/new \
        -I $FROMDIR/input/manual \
        -I $FROMDIR/Documentation \
        -I $FROMDIR/Documentation/included  \
        -I $FROMDIR/Documentation/pictures \
        -o $TODIR/out \
        $FROMDIR/Documentation/$DIRECTORY/$NAME.itely
BOOKRC=$?
if [ $BOOKRC != 0 ]; then
  echo "Lilypond-book returned code $BOOKRC"
  exit $BOOKRC
fi

echo Running RefCheck
python $REFCHECK

cd $DOCDIR
if test -f $TODIR/out/$NAME.texi; then
  echo Running texi2html
  cat $DOCDIR/macros.itexi $TODIR/out/$NAME.texi > $TODIR/$NAME.texi
  $TEXI2HTML \
    --no-validate \
    --output=$TODIR/out/$NAME.html \
    --I=$TODIR/out \
    $TODIR/$NAME.texi
fi

read -p "delete files? (y/n): "
if [ "$REPLY" = "y" ]; then
  echo "deleting files"
  rm -rf $TODIR
fi
