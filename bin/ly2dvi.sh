#!/bin/sh
#
# Script to make a LaTeX file for Lilypond
#
# Written by Jan Arne Fagertun <Jan.A.Fagertun@energy.sintef.no>
#  Sat Nov 22 22:26:43 CET 1997
#
#  Original LaTeX file made by Mats Bengtsson, 17/8 1997
#

VERSION="0.8"
NAME=ly2dvi.sh
IDENTIFICATION="$NAME $VERSION" 
NOW=`date`
echo "$IDENTIFICATION" 1>&2

# NEWS

# 0.8	- Trap Lilypond segmentation fault
#	- Function for cleanup
#	- Trap line
#	- More human-readable variables
#	- Some logics concerning rc-files

# 0.7
#	- Improved Lilypond error checking
#	- Output orientation (landscape...). Overrides mudela file
#	  variable orientation="landscape";
#	- Paper width and heigth put into variables (only A4!)
#	- Adjusted top margin to default.....
#
#TODO
#	- Include more papersizes.
#	- Manual page.......
# 0.6.jaf2
# 	- LILYINCLUDE update
#
# 0.6.hwn1
# 	- handle LILYINCLUDE
#       - --output
#
# 0.6
#	- Source rc-files, if present. Files are:
#	  /usr/local/share/lilyrc /etc/lilyrc $HOME/.lilyrc ./.lilyrc
#	- tee output from Lilypond
#	- Handles margins for A4 paper (equal on both sides)
#	- new option -s (--separate) for one LaTeX run per file,
#	  else all files are run together

# 0.5
#	- More useful ("two-level") debug.
#	- The Q&D hack to find file names and not handling \include
#	  is replaced by grabbing output file names from Lilypond.
#	= Detects multiple output files - adds them when running
#	  LaTeX.
#	- Works with multiple input files - no matter if they are
#	  (a mix of) input to or output from Lilypond.
#
#TODO
#	- Still no margins handling.
#	- We have to discuss how to handle multiple output files
#	  from Lilypond - 'ly2dvi standchen' gives a rather odd
#	  result....

# 0.4.1
#	- Always exit after printing help info
# 0.4
#	- Changes to ensure for more strict grep'ing of parameters
#	  Thanks to from G.B.Stott@bolton.ac.uk
#	- More efficient use of sed -e 's///' -e 's///'
#	  Thanks to Johan Vromans <jvromans@squirrel.nl> and GBS
#	- Ask tex for location of titledefs.tex (Thanks to JV)
#	- Accept only exact match of "\def\mudelacomposer{"
#	  (or whatever mudela* defined in titledefs.tex)
#	- Even more efficient use of sed (Thanks to JV)
#	- Default file name for single output file implemented.
#	- Moved help into function - finally included from 0.1.jcn1
#
#TODO
#	- Still doesn't handle \include
#	- The Q&D for finding output file name from the sequence of
#	  \paper \output \midi \output really needs to be looked at.
#	  I have improved it a lot, but it's only capable of finding
#	  one (the last) file name.
#	  Well, I have to rewrite this entirely to handle \include,
#	  then I can fix it.
#	- Still no margins handling.
#
#WARNING
#	- Some lines of output from lilypond do NOT start
#	  at first character position, therefore I have removed "^"
#	  in sed'ing and grep'ing.

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
# Clean up
#
cleanup() {
  $debug_echo "("$LF")("$FN")("$LOGFILE")"
  if [ "$KEEP" != "Y" ]
  then
    [ -n "$LF" -a -f "$LF" ]           && rm -f $LF
    [ -n "$LOGFILE" -a -f "$LOGFILE" ] && rm -f $LOGFILE
    [ -n "$FN" ]                       && rm -f $FN.*
  fi
}
#
# print usage
#
help() {
  cat << EOF
Generate dvi file from mudela or lilypond output
Usage: $0 [options] file[s]

Options:
  -D, --debug           set debug mode
  -O, --orientation=    set orientation (landscape or portrait (default))
  -o, --output=         set output directory
  -h, --help            this help text
  -k, --keep            keep LaTeX file
  -l, --language=       give LaTeX language (babel)
  -p, --papersize=      give LaTeX papersize (eg. a4paper)
  -s, --separate        run all files separately through LaTeX

  files may be (a mix of) input to or output from lilypond(1)
EOF
}
#
# Trap function (cleanup)
#
trap cleanup 0 9 15
#
# Various defaults
#
[ -z "$TMP" ] && TMP=/tmp
if [ ! -d $TMP ]
then
  $debug_echo $IDENTIFICATION": temporary directory "$TMP" not found, set to /tmp"
  TMP=/tmp
fi
LOGFILE=$TMP/lilylog.$$			# Logfile for lilypond
PWIDTH=600;				# Width of A4 paper!
PHEIGTH=830;                            # Heigth of A4 paper!
#
# RC-files ?
#
for D in /usr/local/share/ /etc/ $HOME/. ./.
do
  RCfile=$D"lilyrc"
  [ -f $RCfile ] && . $RCfile
done
fORI=$ORIENTATION
fLNG=$LANGUAGE
fPSZ=$PAPERSIZE
unset ORIENTATION LANGUAGE PAPERSIZE
# 
# Keywords defined in titledefs.tex
#
TF=`kpsewhich -n tex tex titledefs.tex`
if [ -n $TF ]
then
  TF=/usr/lib/texmf/tex/lilypond/titledefs.tex
fi
MU_DEF=""
if [ -f $TF ]
then
  MU_DEF=`egrep "^.newcommand...mudela" $TF | \\
    sed -e 's/^.newcommand...//' -e 's/\\}.*$//'`
fi

if [ -z "$MU_DEF" ]
then
  MU_DEF="mudelatitle mudelasubtitle mudelacomposer \
          mudelaarranger mudelainstrument"
fi

#
# debugging
#
debug_echo=true
#
# All files in one LaTeX run
#
SEPFILE=N
#
# Find command line options and switches
#
# "x:" x takes argument
#
switches="DO:hkl:o:p:s\?"
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
      [ $debug_echo = echo ] && set -x
      debug_echo=echo
      ;;
    O  )
      ORIENTATION=$OPTARG
      ;;
    h  )
      help;
      exit 0
      ;;
    k  )
      KEEP=Y
      ;;
    l  )
      LANGUAGE=$OPTARG
      ;;
    o  )
      OUTPUTDIR=$OPTARG
      ;;
    p  )
      PAPERSIZE=$OPTARG
      ;;
    s  )
      SEPFILE=Y
      ;;
    \? )
      help;
      exit -1
      ;;
    # a long option!
    -)
      $debug_echo "long option: \`$OPTARG'"
      case "$OPTARG" in
        D*|-D*)
          [ $debug_echo = echo ] && set -x
          debug_echo=echo
          ;;
        h*|-h*)
          help;
	  exit 0
          ;;
        k*|-k*)
          KEEP=Y
          ;;
        l*|-l*)
          LANGUAGE=`echo $OPTARG | sed -e s/"^.*="//`
          ;;
        p*|-p*)
          PAPERSIZE=`echo $OPTARG | sed -e s/"^.*="//`
          ;;
	or*|-or*)
	  ORIENTATION=`echo $OPTARG | sed -e s/"^.*="//`
	  ;;
	ou*|-ou*)
	  OUTPUTDIR=`echo $OPTARG | sed -e s/"^.*="//`
	  ;;
        s*|-s*)
      	  SEPFILE=Y
          ;;
        *|-*)
          echo $0": illegal option -- "$OPTARG;
          help;
          exit -1
          ;;
      esac
  esac
done
shift `expr $OPTIND - 1`
#
# Input file name
#
if [ "$1" = "" ]
then
  help
  $debug_echo $IDENTIFICATION": No input file name given"
  exit 1
fi
#
startFile(){
#
# LaTeX file name
#
BN=`basename $File .tex`
FN=$BN.$$
if [ "$KEEP" != "Y" ]
then
  LF=$TMP/$FN.tex
else
  LF=$FN.tex
fi
#
# Find:
#   paper size        (PAPERSIZE, overridden by command line option -p)
#   paper orientation (ORIENTATION, overridden by option -o)
#   language          (LANGUAGE, overridden by option -l)
#   textwidth
#
eval `sed -n \\
  -e 's/\\\\def\\\\mudelapapersize{\([^}]*\).*$/fPSZ=\1;/p' \\
  -e 's/\\\\def\\\\mudelaorientation{\([^}]*\).*$/fORI=\1;/p' \\
  -e 's/\\\\def\\\\mudelalanguage{\([^}]*\).*$/fLNG=\1;/p' \\
  -e 's/\\\\def\\\\mudelapaperlinewidth{\([^}]*\).*$/TWN=\1;/p' \\
    $File`
if [ -z "$PAPERSIZE" ]
then
  PAPERSIZE=$fPSZ
fi
if [ -n "$PAPERSIZE" ]
then
  PAPEROPT=$PAPERSIZE
fi
#
if [ -z "$ORIENTATION" ]
then
  ORIENTATION=$fORI
fi
if [ -n "$ORIENTATION" ]
then
  if [ -z "$PAPEROPT" ]
  then
    PAPEROPT=$ORIENTATION
  else
    PAPEROPT=$PAPEROPT,$ORIENTATION
  fi
fi
#
if [ -n "$PAPEROPT" ]
then
  PAPER="["$PAPEROPT"]"
fi
#
if [ -z "$LANGUAGE" ]
then
  LANGUAGE=$fLNG
fi
if [ -n "$LANGUAGE" ]
then
  LLNG="\usepackage["$LANGUAGE"]{babel}"
else
  LLNG="%"
fi

#
# Find textwidth
#
if [ -n "$TWN" ]
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
TWp=`echo $TW | sed -e 's/\..*$//'`
PWp=$PWIDTH
if [ "$ORIENTATION" = "landscape" ]
then
  PWp=$PHEIGTH
fi
MARG=`expr $PWp - $TWp`
MARG=`expr $MARG / 2`"pt"
#
# Geometry: /var/lib/texmf/latex/geometry/geometry.dvi
#
#
# Write LaTeX file
#
cat << EOF > $LF
% Creator: $IDENTIFICATION
% Automatically generated from  $IF, $NOW

\documentclass$PAPER{article}
\nonstopmode
$LLNG
\usepackage{geometry}
\usepackage[T1]{fontenc}
%\addtolength{\oddsidemargin}{-1cm}
%\addtolength{\topmargin}{-1cm}
\setlength{\textwidth}{$TW}
%\geometry{width=$TW, left=$MARG, top=1cm}
\geometry{width=$TW, left=$MARG}
\input lilyponddefs
\input titledefs
\begin{document}
EOF
#
# Include \def\mudela-definitions
#
for L in $MU_DEF
do
  LL=`egrep '^\\\\def.'$L'{' $OF`
  if [ "$LL" != "" ]
  then
    LLL=`echo $LL | sed -e 's/}.*$//' -e 's/.*{//'`
    if [ "$LLL" != "" ]
    then
      echo '\'$L'{'$LLL'}%'                                >> $LF
    fi
  fi
done
#
cat << EOF >> $LF
\makelilytitle
EOF
}
#
# Conclusion
#
endFile(){
cat << EOF >> $LF
\vfill\hfill{(\LilyIdString)}
\end{document}
EOF
#
# Run LaTeX
#
latex $LF || exit 5
#
# Rename dvi file
#
if [ -f $FN.dvi ]
then
    RESULT=$BN.dvi
    [ -n "$OUTPUTDIR" ] && RESULT="$OUTPUTDIR/$RESULT"
    cp $FN.dvi $RESULT
fi
#
# Clean up
#
cleanup
#
# Output some info
#
cat << EOF

$IDENTIFICATION: dvi file name is $RESULT

EOF
}

# ugh. GF is side-effect.
findInput() {
# should check for LILYINCLUDE
  for lypath in "." `echo $LILYINCLUDE| sed 's/:/ /g'`
  do
    if [ -f "$lypath/$1" ]
    then
      GF="$lypath/$1"
      return	    
    fi

    if [ -f "$lypath/$1.ly" ]
    then
      GF="$lypath/$1.ly"
      return
    fi
  done
  $debug_echo $IDENTIFICATION": Input file "$GF" not found"
  echo $NAME": Input file "$GF" not found"                       1>&2
  exit 2
}
#
# Loop through all files
#
for GF in $*
do
    findInput $GF

  #
  # Check whether the file is input to or output from lilypond
  #
  L1=`head -1 $GF` 
  OP=`echo $L1 | grep "^% Creator: GNU LilyPond"`
  if [ -n "$OP" ]
  then
    #
    # OK - it's the output from lilypond.
    #
    # Get lilypond source file name
    #
    OF=$GF
    IFL=`grep mudelafilename $OF`
    if [ "$IFL" != "" ]
    then
      IF=`echo $IFL | sed -e 's/.*{//' -e 's/}*.$//'`
      #
      # Check if source file exists
      #
      if [ ! -f $IF ]
      then
        $debug_echo $IDENTIFICATION": Mudela file not found."
        TW=15.5cm
      fi
    else
      $debug_echo $IDENTIFICATION": Mudela file name not found."
      TW=15.5cm
    fi
  else
    #
    # I have to assume this is the lilypond input file
    # Find output file name, if defined
    #
    IF=$GF
    #
    # Run lilypond
    # Grab output file names
    #
    $debug_echo "lilypond "$IF

    lilypond $IF 2>&1  | tee $LOGFILE
    OF=`egrep '^TeX output to ' $LOGFILE | \\
        sed -e 's/TeX output to//' -e 's/\.\.\.//'`
    $debug_echo "==> "$OF
    STATUS=`egrep -i "error|segmentation" $LOGFILE`
    echo $STATUS
    if [ ! -z "$STATUS" ]
    then
      exit 10
    fi
  fi
  #
  # "Spin through" all the files
  #
  for File in $OF
  do
    $debug_echo "--- "$File
    #
    # Check if output file is generated
    #
    if [ ! -f $File ]
    then
      $debug_echo $IDENTIFICATION": hmm, I could not find the output file "$File
      exit 4
    fi
    #
    # Is this the first file?
    #
    if [ -z "$FFile" ]
    then
      FFile=$File
      startFile
    fi
    cat << EOF >> $LF
\input{$File}
EOF
    if [ $SEPFILE = Y ]
    then
      FFile=""
      endFile
    fi
  done
done
if [ $SEPFILE = N ]
then
  endFile
fi
#
# OK - finished
#
