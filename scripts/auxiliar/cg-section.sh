#!/bin/sh

#  Build html versions of sections of lilypond Contributors' Guide
#
#  Usage:  cg-section.sh SECTION
#
#   where SECTION is the section to be built.
#
#   For example, CG 4 would be built by
#       cg-section.sh doc-work
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
#

#
#  Customize the file here
#
FROMDIR="$HOME/lilypond"
DOCDIR="$HOME/lilypond/tempdocs"
TODIR="$DOCDIR/contributor"
TEXI2HTML="texi2html"
REFCHECK="$FROMDIR/scripts/auxiliar/ref_check.py"

NAME=$1

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

echo Running RefCheck
python $REFCHECK

cd $DOCDIR
echo Running texi2html
cat $DOCDIR/macros.itexi $FROMDIR/Documentation/contributor/$NAME.itexi > $TODIR/$NAME.texi
$TEXI2HTML \
  --no-validate \
  --output=$TODIR/out/$NAME.html \
  --I=$FROMDIR/Documentation \
  --I=$TODIR/out \
  $TODIR/$NAME.texi

read -p "delete files? (y/n): "
if [ "$REPLY" = "y" ]; then
  echo "deleting files"
  rm -rf $TODIR
fi
