#!/bin/sh
#
# Script to make a LaTeX file for Lilypond
#
# Written by Jan Arne Fagertun <Jan.A.Fagertun@energy.sintef.no>
#  Sat Nov 22 22:26:43 CET 1997
#
#  Original LaTeX file made by Mats Bengtsson, 17/8 1997
#

VERSION="0.3.hwn1"
IDENTIFICATION="lytodvi.sh $VERSION" 
NOW=`date`
echo "$IDENTIFICATION" 1>&2

# NEWS
# 0.3.hwn1
# 	- add "Creator: " line to output
#
# 0.3
#	- multiple input files to make score from several files
#	  (extra files assumed to be Lilypond output)
#	- cp dvi-file instead of mv, to allow for xdvi reload
#	- check for illegal long options
#	- put in pt in text width, if not given
#	- put in \nonstopmode in LaTeX file
#	- restored LaTeX exit value check
 
# 0.2.1
#	- temporarily omit LaTeX exit value check
#	- remove ALL temporary files

# 0.2
#	- fix for solaris          - included from 0.1.jcn1
#	- long option hack         - included from 0.1.jcn1 - modified
#	- moved help into function - NOT included from 0.1.jcn1 yet
#	- various improvements     - included from 0.1.jcn1
#	- find mudela definitions from titledefs.tex
#	- find papersize from lilypond output file (mudelapapersize),
#	  overridden by option '-p size' or '--papersize=size'
#	- option -l lang or --language=lang overrides
#	  lilypond output file definition (mudelalanguage)
#	- textwidth from lilypond output file (mudelapaperlinewidth)

# 0.1.jcn1
#	- fix for solaris
#	- long option hack
#	- moved help into function

#
# Exit value, when needed
#
EXIT=0

#
# Keywords defined in titledefs.tex
#   Isn't there a way to ask LaTeX for the location of titledefs.tex?
#
TF=/usr/lib/texmf/tex/lilypond/titledefs.tex
MU_DEF=""
if [ -f $TF ]
then
  MU_DEF=`egrep "^.newcommand...mudela" $TF | \\
    sed -e s/^.newcommand...//g | sed -e "s/\\}.*$//"`
fi

if [ "$MU_DEF" = "" ]
then
  MU_DEF="mudelatitle mudelasubtitle mudelacomposer \
          mudelaarranger mudelainstrument"
fi

#
# debugging
#
# debug_echo=echo
debug_echo=true

#
# Find command line options and switches
#
# "x:" x takes argument
#
switches="Dhkl:p:\?"
options=""
#
# ugh, "\-" is a hack to support long options
# while getopts \-:$options$switches O
# must be in double quotes for bash-2.0
while getopts "\-:$options$switches" O
do
  $debug_echo "O: \`$O'"
  $debug_echo "arg: \`$OPTARG'"
  case $O in
    D  )
      set -x
      ;;
    h  )
      HELP=Y
      ;;
    k  )
      KEEP=Y
      ;;
    l  )
      LNG=$OPTARG
      ;;
    p  )
      PSZ=$OPTARG
      ;;
    \? )
      HELP=Y
      ;;
    # a long option!
    -)
      $debug_echo "long option: \`$OPTARG'"
      case "$OPTARG" in
        h*|-h*)
          HELP=Y
          ;;
        k*|-k*)
          KEEP=Y
          ;;
        l*|-l*)
          LNG=`echo $OPTARG | sed -e s/"^.*="//`
          ;;
        p*|-p*)
          PSZ=`echo $OPTARG | sed -e s/"^.*="//`
          ;;
        D*|-D*)
          set -x;
          ;;
        *|-*)
          echo $0": illegal option -- "$OPTARG;
          EXIT=-1;
          HELP=Y
          ;;
      esac
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
    cat << EOF

$0 - no input file name given
EOF
    EXIT=1
    HELP=Y
  fi
fi
GF=$1
#
# Check if input file exists...
#
if [ "$HELP" != "Y" ]
then
  if [ ! -f $GF ]
  then
    GF=$1.ly
    if [ ! -f $GF ]
    then
      cat << EOF

$0 - input file $GF not found
EOF
      EXIT=2
      HELP=Y
    fi
  fi
fi

#
# print usage
#
#help() {
if [ "$HELP" = "Y" ]
then
  cat << EOF
Generate dvi file from mudela or lilypond output
Usage: $0 [options] file

Options:
  -D, --debug           set debug mode
  -h, --help            this help text
  -k, --keep            keep LaTeX file
  -l, --language=       give LaTeX language (babel)
  -p, --papersize=      give LaTeX papersize (eg. a4paper)

  file may be input to or output from lilypond(1)
EOF
  exit $EXIT
fi
#}

#
# More files?
#
shift 1

#
# Check whether input file is the input to or output from lilypond
#
L1=`head -1 $GF` 
OP=`echo $L1 | grep "^% Creator: GNU LilyPond"`
if [ "$OP" != "" ]
then
  #
  # OK - it's the output from lilypond.
  # Get lilypond source file name
  #
  OF=$GF
  IFL=`grep mudelafilename $OF`
  if [ "$IFL" != "" ]
  then
    IF=`echo $IFL | sed -e s/^.*{// | sed -e s/"}*.$"//`
    #
    # Check if source file exists
    #
    if [ ! -f $IF ]
    then
      cat << EOF

$0 - mudela file not found.

EOF
      TW=15.5cm
    fi
  else
    cat << EOF

$0 - mudela file name not found.

EOF
    TW=15.5cm
  fi
else
  #
  # I have to assume this is the lilypond input file
  # Find output file name, if defined
  #
  IF=$GF
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
      OF=`echo $OF2 | sed -e "s/\\";.*$//" | sed -e "s/^.*\\"//"`
    fi
    if [ "$OF2" = "" ]
    then
      OF=lelie.tex
    fi
  fi
  #
  # Remove the output file, to avoid being misled by an old one
  #
#  if [ -f $OF ]
#  then
#    rm $OF
#  fi
  #
  # Run lilypond - exit if unsuccessfull
  #
  lilypond $IF || exit 3
  #
  # Check if output file is generated
  #
  if [ ! -f $OF ]
  then
    cat << EOF

$0 - hmm, I could not find the output file $OF

EOF
    exit 4
  fi
fi
#
# Find textwidth 
#
if [ "$IF" != "" ]
then
  if [ -f $IF ]
  then
    TWL=`grep linewidth $IF`
    TWS=`echo $TWL | grep -v "^%"`
    if [ "$TWS" != "" ]
    then
      TW=`echo $TWS | sed -e s/^.*=//  | sed -e s/";.*$"// | \\
          sed -e s/.mm/mm/ | sed -e s/.cm/cm/  | sed -e s/.p/p/`
      case $TW in
        *mm)
          ;;
        *cm)
          ;;
        *pt)
          ;;
        *)
          TW=$TW"pt"
          ;;
      esac
    else
      TW=15.5cm
    fi
  fi
fi
#
# LaTeX file name
#
if [ "$KEEP" != "Y" ]
then
  if [ "$TMP" = "" ]
  then
    TMP=/tmp
  fi
  if [ ! -d $TMP ]
  then
    cat << EOF

$0 - temporary directory $TMP not found, set to /tmp

EOF
    TMP=/tmp
  fi
#
  BN=`basename $OF .tex`
  FN=$BN.$$
  LF=$TMP/$FN.tex
else
  BN=`basename $OF .tex`
  FN=$BN.$$
  LF=$FN.tex
fi
#
# Find if a paper size is defined
#
if [ "$PSZ" = "" ]
then
  PSZ=`egrep ".def.mudelapapersize" $OF | \\
        sed -e s/.def.mudelapapersize//  | \\
        sed -e s/^{// | sed -e s/}.*$//`
fi

if [ "$PSZ" != "" ]
then
  PAPER="["$PSZ"]"
fi

#
# Find if a language is defined
#
if [ "$LNG" = "" ]
then
  LNG=`egrep ".def.mudelalanguage" $OF | \\
        sed -e s/.def.mudelalanguage//  | \\
        sed -e s/^{// | sed -e s/}.*$//`
fi
if [ "$LNG" != "" ]
then
  LLNG="\usepackage["$LNG"]{babel}"
else
  LLNG="%"
fi

#
# Find if a textwidth is defined
#
TWN=`egrep ".def.mudelapaperlinewidth" $OF | \\
        sed -e s/.def.mudelapaperlinewidth//  | \\
        sed -e s/^{// | sed -e s/}.*$//`
if [ "$TWN" != "" ]
then
  TW=$TWN
  case $TW in
    *mm)
      ;;
    *cm)
      ;;
    *pt)
      ;;
    *)
      TW=$TW"pt"
      ;;
  esac
  $debug_echo "Text width = "$TW
fi

#
# Write LaTeX file
#
cat << EOF > $LF
% Creator: $IDENTIFICATION
% Automatically generated from  $IF, $NOW

\documentclass$PAPER{article}
\nonstopmode
$LLNG
\usepackage[T1]{fontenc}
\addtolength{\oddsidemargin}{-1cm}
\addtolength{\topmargin}{-1cm}
\setlength{\textwidth}{$TW}
\input lilyponddefs
\input titledefs
\begin{document}
EOF
#
# Include \def\mudela-definitions
#
for L in $MU_DEF
do
  LL=`grep ".def."$L $OF`
  if [ "$LL" != "" ]
  then
    LLL=`echo $LL | sed -e s/^.def.$L// | \\
         sed -e s/^{// | sed -e s/}.*$//`
    if [ "$LLL" != "" ]
    then
      echo '\'$L'{'$LLL'}%'                                    >> $LF
    fi
  fi
done
#
cat << EOF >> $LF
\makelilytitle
\input{$OF}
EOF
for EX in $*; do echo "\input{"$EX"}%" >> $LF; done
cat << EOF >> $LF
\vfill\hfill{(\LilyIdString)}
\end{document}
EOF
#
# Run LaTeX
#
latex $LF || exit 5
#latex $LF
cat << EOF


Take care - LaTeX exit value check temporarily omitted!


EOF
#
# Rename dvi file
#
if [ -f $FN.dvi ]
then
  cp $FN.dvi $BN.dvi
fi
#
# Clean up
#
if [ "$KEEP" != "Y" ]
then
  rm $LF $FN.*
fi
#
# Output some info
#
cat << EOF

$0 - dvi file name is $BN.dvi

EOF
# OK - finished
