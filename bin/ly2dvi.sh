#!/bin/sh
#
# Script to make a LaTeX file for Lilypond
#
# Written by Jan Arne Fagertun <Jan.A.Fagertun@energy.sintef.no>
#  Sat Nov 22 22:26:43 CET 1997
#
#  Original LaTeX file made by Mats Bengtsson, 17/8 1997
#

VERSION="0.5"
IDENTIFICATION="lytodvi.sh $VERSION" 
NOW=`date`
echo "$IDENTIFICATION" 1>&2

# NEWS
# 0.5.hwn1
#	- do tee of output.
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
# print usage
#
help() {
  cat << EOF
Generate dvi file from mudela or lilypond output
Usage: $0 [options] file[s]

Options:
  -D, --debug           set debug mode
  -h, --help            this help text
  -k, --keep            keep LaTeX file
  -l, --language=       give LaTeX language (babel)
  -p, --papersize=      give LaTeX papersize (eg. a4paper)

  files may be (a mix of) input to or output from lilypond(1)
EOF
}
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
      if [ $debug_echo = echo ]
      then
        set -x
      fi
      debug_echo=echo
      ;;
    h  )
      help;
      exit 0
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
      help;
      exit -1
      ;;
    # a long option!
    -)
      $debug_echo "long option: \`$OPTARG'"
      case "$OPTARG" in
        h*|-h*)
          help;
	  exit 0
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
          if [ $debug_echo = echo ]
          then
            set -x
          fi
          debug_echo=echo
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
for GF in $*
do
  #
  # Check if input file exists...
  #
  if [ ! -f $GF ]
  then
    GF=$GF.ly
    if [ ! -f $GF ]
    then
      $debug_echo $IDENTIFICATION": Input file "$GF" not found"
      exit 2
    fi
  fi
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

    lilypond $IF 2>&1  | tee /tmp/lilylog.$$
    OF=`cat /tmp/lilylog.$$| egrep '^TeX output to ' | \\
        sed -e 's/TeX output to//' -e 's/\.\.\.//'`
    $debug_echo "==> "$OF
  fi
  #
  # Check if output file is generated
  #
  for File in $OF
  do
    $debug_echo "--- "$File
    if [ ! -f $File ]
    then
      $debug_echo $IDENTIFICATION": hmm, I could not find the output file "$File
      exit 4
    fi
    if [ -z "$FFile" ]
    then
      FFile=$File
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
          $debug_echo $IDENTIFICATION": temporary directory "$TMP" not found, set to /tmp"
          TMP=/tmp
        fi
      #
        BN=`basename $FFile .tex`
        FN=$BN.$$
        LF=$TMP/$FN.tex
      else
        BN=`basename $FFile .tex`
        FN=$BN.$$
        LF=$FN.tex
      fi
      #
      # Find:
      #   paper size (PSZ, overridden by command line option -p)
      #   language   (LNG, overridden by command line option -l)
      #   textwidth
      #
      eval `sed -n \\
        -e 's/\\\\def\\\\mudelapapersize{\([^}]*\).*$/fPSZ=\1;/p' \\
        -e 's/\\\\def\\\\mudelalanguage{\([^}]*\).*$/fLNG=\1;/p' \\
        -e 's/\\\\def\\\\mudelapaperlinewidth{\([^}]*\).*$/TWN=\1;/p' \\
          $OF`
      if [ "$PSZ" = "" ]
      then
        PSZ=$fPSZ
      fi
      if [ "$PSZ" != "" ]
      then
        PAPER="["$PSZ"]"
      fi
      #
      if [ "$LNG" = "" ]
      then
        LNG=$fLNG
      fi
      if [ "$LNG" != "" ]
      then
        LLNG="\usepackage["$LNG"]{babel}"
      else
        LLNG="%"
      fi

      #
      # Find textwidth
      #
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
    fi
    cat << EOF >> $LF
\input{$File}
EOF
  done
done
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

$IDENTIFICATION: dvi file name is $BN.dvi

EOF
# OK - finished
