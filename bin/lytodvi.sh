#!/bin/sh
#
# Written by Jan Arne Fagertun <Jan.A.Fagertun@energy.sintef.no>
#  Sat Nov 22 22:26:43 CET 1997
#
# Script to make a latex file for Lilypond
#
# Find options given
#

# should use header info or locale --HWN
LANGUAGE="english"
VERSION="0.0"


echo "lytodvi.sh $VERSION" 1>&2
while getopts Dhk O
do
  case $O in
    D  ) set -x;DEBUG="-v";;
    h  ) HELP=Y;;
    k  ) KEEP=Y;;
    \? ) HELP=Y;;
  esac
done
shift `expr $OPTIND - 1`

#
# Input file name
#
if [ "$HELP" != "Y" ]
then
  if [ "$1" = "" ]
  then
    echo $0" - no input file name given"                           1>&2
    HELP=Y
  fi
fi
#
# Check if input file exists...
#
if [ "$HELP" != "Y" ]
then
  if [ ! -f $1 ]
  then
    echo $0" - input file not found    "                           1>&2
    HELP=Y
  fi
fi
#
# Help info
#
if [ "$HELP" = "Y" ]
then
  echo
  echo "usage : "$0" [-D -h -k] file"
cat << EOF
  -D = set debug mode
  -h = this help text
  -k = keep latex file
  file may be input to or output from lilypond
EOF
  echo
  exit 1
fi
#
# Check whether input file is the input to or output from lilypond
#
L1=`head -1 $1` 
OP=`echo $L1 | grep "^% Creator: GNU LilyPond"`
if [ "$OP" != "" ]
then
  #
  # OK - it's the output from lilypond.
  # Get lilypond source file name
  #
  OF=$1
  IFL=`grep mudelafilename $OF`
  if [ "$IFL" != "" ]
  then
    IF=`echo $IFL | sed -e s/^.*{// | sed -e s/"}*.$"//`
    #
    # Check if source file exists
    #
    if [ ! -f $IF ]
    then
      echo $0" - mudela file not found."
      TW=15.5cm
    fi
  else
    echo $0" - mudela file name not found."
    TW=15.5cm
  fi
else
  #
  # I have to assume this is the lilypond input file
  # Find output file name, if defined
  #
  IF=$1
  OFS=`egrep "paper|midi|output" $IF`
  OF1=`echo $OFS | sed -e s/".midi.*$"// | sed -e s/"^.*paper"//`
  if [ "$OF1" = "" ]
  then
    OF1=`echo $OFS | sed -e s/"^.*paper"// | sed -e s/".midi.*$"//`
  fi
  if [ "$OF1" = "" ]
  then
    OF=lelie.tex
  else
    OF2=`echo $OF1 | grep output`
    if [ "$OF2" = "" ]
    then
      OF=lelie.tex
    else
      OF=`echo $OF2 | sed -e s/'";.*'$// | sed -e s/^'.*"'//`
    fi
    if [ "$OF2" = "" ]
    then
      OF=lelie.tex
    fi
  fi
  #
  # Run lilypond - exit if unsuccessfull
  #
  lilypond $IF || exit 3
fi
#
# Find textwidth
#
if [ "$IF" != "" ]
then
  if [ -f $IF ]
  then
    TWL=`grep linewidth $IF`
    if [ "$TWL" != "" ]
    then
      TW=`echo $TWL | sed -e s/^.*=//  | sed -e s/";.*$"// | \\
                      sed -e s/.mm/mm/ | sed -e s/.cm/cm/  | sed -e s/.p/p/`
    else
      TW=15.5cm
    fi
  fi
fi
#
# More parameters from input file
#
# Well - seems like some more info is needed:
#   Arranger   (I use mudelacopyright now)
#   Instrument (I use mudelaremarks now)
#
# Latex file
#
if [ "$KEEP" != "Y" ]
then
  if [ "$TMP" = "" ]
  then
    TMP=/tmp
  fi
  if [ ! -d $TMP ]
  then
    echo $0" - temporary directory "$TMP" not found, set to /tmp"
    TMP=/tmp
  fi
#
  LF=$TMP/$OF.tex
else
  LF=$OF.tex
fi
#
# Should copy a "header file" instead....
#
cat << EOF > $LF
\documentclass[a4paper]{article}
\usepackage[$LANGUAGE]{babel}
\usepackage[T1]{fontenc}
\addtolength{\oddsidemargin}{-1cm}
\addtolength{\topmargin}{-1cm}
EOF

#  why two <<EOF constructs? --HWN
echo "\setlength{\textwidth}{"$TW"}"                           >> $LF
cat << EOF >> $LF
\input lilyponddefs
\input titledefs
\begin{document}
EOF
    
#
# Include \def\mudela-definitions - strip off \def\mudela.
#   Hmm - why not just change titledefs?
#
for L in mudelatitle mudelasubtitle mudelacomposer mudelaarranger mudelainstrument
do
  LL=`grep $L $OF`
  if [ "$LL" != "" ]
  then
    echo $LL | sed -e s/d.*mudela//g >> $LF
  fi
done
#
echo "\makelilytitle"                                          >> $LF
echo "\input{"$OF"}"                                           >> $LF
echo "\vfill\hfill{(\LilyIdString)}"                           >> $LF
echo "\end{document}"                                          >> $LF

#
# Run latex
#
latex $LF
#
# Clean up
#
if [ "$KEEP" != "Y" ]
then
  rm $LF
fi
