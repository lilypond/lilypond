#!/bin/sh
#
# Script to make a LaTeX file for Lilypond
#
# Written by Jan Arne Fagertun <Jan.A.Fagertun@energy.sintef.no>
#  Sat Nov 22 22:26:43 CET 1997
#
# $Id: ly2dvi.sh,v 1.6 1998/09/15 17:22:43 fred Exp $
#
#  Original LaTeX file made by Mats Bengtsson, 17/8 1997
#

VERSION="0.12.jcn1"
NAME=ly2dvi.sh
IDENTIFICATION="$NAME $VERSION" 
NOW=`date`
echo "$IDENTIFICATION" 1>&2

# TODO:
#  prevent orphaned "Lily is here" strings

# NEWS

# 0.12.jcn1
#  - mudelaDefs really fixed (sorry, PC)

# 0.12
#  - -S/--sourcedir switch

#
#0.11.jcn3
# - mudelaDefs fixes, (thanks PC)
#0.11.jcn2
# - pass -I, --include to Lily
# - bf: split $LILYINCLUDE and try all dirs
# - bf: geometry left/top
#
#0.11.pc
# - fix backslash gobbling, reduce number of separate processes used
# - require readable rc files, allow rc file in /usr/local/share/lilypond/lilyrc
# - use latex path, not tex path
# - Don't override if kpsepath returns non-NULL
# - Don't clobber x.tex if it wasn't created by ly2dvi
#
#
#0.11.jcn1
# - find .fly files too
#
#0.11.hwn1
# - height vs heigth
# - robustification: give \nonstopmode as LaTeX arg; no hanging if no TeX file.
# - robustification: notice failed cp.

#0.11
#	- more papersizes (thanks Han-Wen) - don't expect LaTeX to accept all...
#	- -W,--Width=       : set paper width  (points)
#       - -H,--Height=      : set paper height (points)
#	- -H and -W is used only when an unknown papersize is to be specified,
#	  and has to be combined with a papersize option known to LaTeX
#NB!	- -F,--headers=     : name of additional LaTeX headers input file.
#NB!      Changed from -H to -F
#	- -d,--dependencies : passed to lilypond


#0.10.jcn1
#	- HEIGHT -> HEIGHT
#	- vertical margins (for a4), same on both sides
#	- textheight from lilypond output file (mudelapapertextheight)
#	- piece titling
#	- mudelapiece, mudelaopus
#
#0.10
#	- -K,--keeplilypond : Keep lilypond output files (default delete)
#	- -k,--keeply2dvi   : Keep ly2dvi   output files (default delete)
#	- -L,--landscape    : Set landscape orientation
#	- -N,--nonumber     : Turn off page numbering (\pagestyle{empty})
#	- Could not reinsert "\usepackage[T1]{fontenc}" because
#	  "los-toros" won't work with it
#	- Ekstra LaTeX headers from input file
#0.9.hwn1
#       - option to remove output of lily
# 0.9
#	- Trap Lilypond abort
#	- Replaced "\usepackage[T1]{fontenc}" with
#	  \usepackage[latin1]{inputenc} (takk, Mats)
#	- Removed "()" around "\LilyIdString" (Janne didn't want it)
# 0.8	- Trap Lilypond segmentation fault
#	- Function for cleanup
#	- Trap line
#	- More human-readable variables
#	- Some logics concerning rc-files
# 0.7
#	- Improved Lilypond error checking
#	- Output orientation (landscape...). Overrides mudela file
#	  variable orientation="landscape";
#	- Paper width and height put into variables (only A4!)
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
#
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
  if [ "$KEEP_LY2DVI_OUTPUT" != "Y" ]
  then
    [ -n "$LatF" -a -f "$LatF" ]       && rm -f $LatF
    [ -n "$LOGFILE" -a -f "$LOGFILE" ] && rm -f $LOGFILE
    [ -n "$FN" ]                       && rm -f $FN.*
    for F in *$$* $TMP/*$$*
    do
      rm -f $F
    done
  fi
  if [ "$KEEP_LILY_OUTPUT" != "Y" ]
  then
    for F in $LILY_OUTPUT_FILES
    do
      [ -f $F ] && rm -f $F
    done
  fi
}

#
# print usage
#
help() {
  cat << EOF
Generate dvi file from mudela or lilypond output
Usage: $0 [OPTION]... [FILE]...

Options:
  -D,--debug           increase verbosity
  -F,--headers=        name of additional LaTeX headers file
  -H,--Height=         set paper height (points) (see manual page)
  -I,--include=DIR     add DIR to search path of LilyPond
  -K,--keeplilypond    keep lilypond output files
  -L,--landscape       set landscape orientation
  -N,--nonumber        switch off page numbering
  -O,--orientation=    set orientation (obsolete - use -L instead) 
  -W,--Width=          set paper width (points) (see manual page)
  -d,--dependencies    tell lilypond make a dependencies file
  -h,--help            this help text
  -k,--keeply2dvi      keep ly2dvi output files
  -l,--language=       give LaTeX language (babel)
  -o,--output=         set output directory
  -p,--papersize=      give LaTeX papersize (eg. a4)
  -s,--separate        run all files separately through LaTeX
  -S,--sourcedir       set sourcedir 

  files may be (a mix of) input to or output from lilypond(1)
EOF
}


#
# SVr4 echo swallows backslashes, and there's no way to turn it off.
# Therefore use Echo whenever backslashes may be important.
# printf is in the posix.2 standard -- which means it's in 
# all modern shells.
#
Echo() {
	printf "%s\n" "$@"
}

#
setPaperZize() {
case "$PAPERSIZE" in
  a0*)
    PWIDTH=2389
    PHEIGHT=3381
    PAPERSIZE=a0paper
    ;;
  a1|a1p*)
    PWIDTH=1690
    PHEIGHT=2389
    PAPERSIZE=a1paper
    ;;
  a2*)
    PWIDTH=1194
    PHEIGHT=1690
    PAPERSIZE=a2paper
    ;;
  a3*)
    PWIDTH=845
    PHEIGHT=1194
    PAPERSIZE=a3paper
    ;;
  a4*)
    PWIDTH=597
    PHEIGHT=845
    PAPERSIZE=a4paper
    ;;
  a5*)
    PWIDTH=423
    PHEIGHT=597
    PAPERSIZE=a5paper
    ;;
  a6*)
    PWIDTH=298
    PHEIGHT=423
    PAPERSIZE=a6paper
    ;;
  a7*)
    PWIDTH=211
    PHEIGHT=298
    PAPERSIZE=a7paper
    ;;
  a8*)
    PWIDTH=305
    PHEIGHT=211
    PAPERSIZE=a8paper
    ;;
  a9*)
    PWIDTH=105
    PHEIGHT=305
    PAPERSIZE=a9paper
    ;;
  a10*)
    PWIDTH=74
    PHEIGHT=105
    PAPERSIZE=a10paper
    ;;
  b0*)
    PWIDTH=2847
    PHEIGHT=4023
    PAPERSIZE=b0paper
    ;;
  b1*)
    PWIDTH=2012
    PHEIGHT=2847
    PAPERSIZE=b1paper
    ;;
  b2*)
    PWIDTH=1423
    PHEIGHT=2012
    PAPERSIZE=b2paper
    ;;
  b3*)
    PWIDTH=1006
    PHEIGHT=1423
    PAPERSIZE=b3paper
    ;;
  b4*)
    PWIDTH=712
    PHEIGHT=1006
    PAPERSIZE=b4paper
    ;;
  b5*)
    PWIDTH=503
    PHEIGHT=712
    PAPERSIZE=b5paper
    ;;
  archA)
    PWIDTH=650
    PHEIGHT=867
    ;;
  archB)
    PWIDTH=867
    PHEIGHT=1301
    ;;
  archC)
    PWIDTH=1301
    PHEIGHT=1734
    ;;
  archD)
    PWIDTH=1734
    PHEIGHT=2602
    ;;
  archE)
    PWIDTH=2602
    PHEIGHT=3469
    ;;
  flsa|flse)
    PWIDTH=614
    PHEIGHT=940
    ;;
  halfletter)
    PWIDTH=397
    PHEIGHT=614
    ;;
  ledger)
    PWIDTH=1229
    PHEIGHT=795
    ;;
  legal)
    PWIDTH=614
    PHEIGHT=1012
    ;;
  letter)
    PWIDTH=614
    PHEIGHT=795
    ;;
  note)
    PWIDTH=542
    PHEIGHT=723
    ;;
  *)
    echo ""
    echo $0": unknown papersize -- "$PAPERSIZE
    echo ""
    ;;
esac
}

doRcFiles()
{
#
# RC-files ?
#
for D in /usr/local/share/ /usr/local/share/lilypond/ /etc/ $HOME/. ./.
do
  RCfile=$D"lilyrc"
  [ -r $RCfile ] && . $RCfile
done
fORI=$ORIENTATION
fLNG=$LANGUAGE
fPSZ=$PAPERSIZE
fLHF=$LATEXHF
unset ORIENTATION LANGUAGE PAPERSIZE LATEXHF
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
LOGFILE=$TMP/lilylog.$$	                # Logfile for lilypond
PAPERSIZE=a4                            # Default papersize name
PWIDTH=597                              # Default paperwidth
PHEIGHT=845                             # Default paperheight
PNUM="%"                                # Page numbering on
LILYOPTS=""                             # Options to lilypond
SOURCEDIR=""
LILYPOND_EXECUTABLE=lilypond
doRcFiles



# Keywords defined in titledefs.tex
#
TF="`kpsewhich -n latex tex titledefs.tex`"
MU_DEF=""
if [ -r "${TF:=/usr/local/share/texmf/tex/lilypond/titledefs.tex}" ]
then
  MU_DEF=`sed -n -e 's/.*newcommand\*{.\(mudela[^}]*\)}.*$/\1/p' "$TF"`
fi

: "${MU_DEF:=mudelatitle mudelasubtitle mudelacomposer \
          mudelameter mudelaopus mudelaarranger mudelapiece \
	  mudelapoet mudelainstrument \
	  }"

#
# debugging
#
debug_echo=:
#
# All files in one LaTeX run
#
SEPFILE=N
#
# Find command line options and switches
#
# "x:" x takes argument
#
switches="DF:H:I:KLNO:W:dhkl:o:p:S:s\?"
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
    F  )
      LATEXHF=$OPTARG
      ;;
    H  )
      PHEIGHT=$OPTARG
      ;;
    I  )
      LILYOPTS="$LILYOPTS -I $OPTARG"
      LILYINCLUDE="$LILYINCLUDE:$OPTARG"
      ;;
    K  )
      KEEP_LILY_OUTPUT=Y
      ;;
    L  )
      ORIENTATION=landscape
      ;;
    O  )
      ORIENTATION=$OPTARG
      ;;
    N  )
      PNUM="\pagestyle{empty}"
      ;;
    W  )
      PWIDTH=$OPTARG
      ;;
    d  )
      LILYOPTS=$LILYOPTS" -d"
      ;;
    h  )
      help;
      exit 0
      ;;
    k  )
      KEEP_LY2DVI_OUTPUT=Y
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
    S  )
      SOURCEDIR=$OPTARG
      ;;
    \? )
      help;
      exit -1
      ;;
    # a long option!
    -)
      $debug_echo "long option: \`$OPTARG'"
      case "$OPTARG" in
        He*|-He*)
          PHEIGHT"`expr \"$OPTARG\" ':' '[^=]*=\(.*\)'`"=
          ;;
        W*|-W*)
          PWIDTH="`expr \"$OPTARG\" ':' '[^=]*=\(.*\)'`"
          ;;
        dep*|-dep*)
          LILYOPTS="$LILYOPTS -d"
          ;;
        d*|-d*)
          [ $debug_echo = echo ] && set -x
          debug_echo=echo
          ;;
        hea*|-hea*)
          LATEXHF="`expr \"$OPTARG\" ':' '[^=]*=\(.*\)'`"
          ;;
        h*|-h*)
          help;
	  exit 0
          ;;
        i*|-i*)
          dir="`expr \"$OPTARG\" ':' '[^=]*=\(.*\)'`"
	  LILYOPTS="$LILYOPTS --include=$dir"
	  LILYINCLUDE="$LILYINCLUDE:$dir"
	  ;;
        keepli*|-keepli*)
          KEEP_LILY_OUTPUT=Y
          ;;
        k*|-k*)
          KEEP_LY2DVI_OUTPUT=Y
          ;;
        land*|-land*)
          ORIENTATION=landscape
          ;;
        lang*|-lang*)
          LANGUAGE="`expr \"$OPTARG\" ':' '[^=]*=\(.*\)'`"
          ;;
	n*|-n*)
	  PNUM="\pagestyle{empty}"
	  ;;
	or*|-or*)
	  ORIENTATION="`expr \"$OPTARG\" ':' '[^=]*=\(.*\)'`"
	  ;;
	ou*|-ou*)
	  OUTPUTDIR="`expr \"$OPTARG\" ':' '[^=]*=\(.*\)'`"
	  ;;
        p*|-p*)
          PAPERSIZE="`expr \"$OPTARG\" ':' '[^=]*=\(.*\)'`"
          ;;
        separa*|-sep*)
      	  SEPFILE=Y
          ;;
	sourcedir*|-sourcedir*)
	  SOURCEDIR=$OPTARG
	  ;;
        *|-*)
          echo "$0: illegal option -- \"$OPTARG\""
          help;
          exit -1
          ;;
      esac
  esac
done
shift `expr $OPTIND - 1`

if [ "$SOURCEDIR" != "" ]; then
# apparently MakeTeXTFM can't handle relative dirs
	SOURCEDIR=`cd $SOURCEDIR; pwd`
	LILYINCLUDE="$SOURCEDIR/init:$SOURCEDIR/mf/out:$LILYINCLUDE"
	TEXINPUTS="$SOURCEDIR/tex:$TEXINPUTS:"
	MFINPUTS="$SOURCEDIR/mf:$MFINPUTS:"
	LILYPOND_EXECUTABLE="$SOURCEDIR/lily/out/lilypond"
	export MFINPUTS LILYPOND_EXECUTABLE TEXINPUTS SOURCEDIR 
fi

#
# Input file name
#
if [ "$1" = "" ]
then
  help
  $debug_echo "$IDENTIFICATION: No input file name given"
  exit 1
fi
# regexp_quote -- quote a string as a regular expression for egrep or sed
regexp_quote(){
	Echo "$@" | sed 's/\([]\[\.*?+^$]\)/\\\1/g'
}
#
#
mudelaDefs(){
# Include \def\mudela-definitions
# The aim here is to pick up the definition for the 
# current file, then any other file.
#
mudelatmp=$TMP/mudelaDefs$$
# Use `cat' to prevent filenames being prepended
# 
cat "$File" $OF | fgrep "$MU_DEF" > $mudelatmp
for L in $MU_DEF
do
    # This converts \def\mudelatitle{fred}
    # to \mudelatitle{fred} or to
    # \def\mudelatitle{fred}
    # and stops after the first one found.
    sed -n '/\\def\\'"$L"'{\([^}]*\)}.*$/{
	s//'"`regexp_quote \"$1\"`"'\\'"$L"'{\1}%/p
	q
    }'  $mudelatmp  >> $LatF
done
rm -f $mudelatmp
}
#
#
startFile(){
#
# LaTeX file name
#
BN=`basename "$File" .tex`
FN="$BN.$$"
if [ "$KEEP_LY2DVI_OUTPUT" != "Y" ]
then
  LatF="$TMP/$FN.tex"
else
  LatF="$FN.tex"
fi
#
# Find:
#   paper size        (PAPERSIZE, overridden by command line option -p)
#   paper orientation (ORIENTATION, overridden by option -o)
#   language          (LANGUAGE, overridden by option -l)
#   textwidth
#
eval `sed -n \\
  -e 's/\\\\def\\\\mudelalanguage{\([^}]*\).*$/fLNG=\1;/p' \\
  -e 's/\\\\def\\\\mudelalatexheaders{\([^}]*\).*$/fLHF=\1;/p' \\
  -e 's/\\\\def\\\\mudelaorientation{\([^}]*\).*$/fORI=\1;/p' \\
  -e 's/\\\\def\\\\mudelapaperlinewidth{\([^}]*\).*$/TWN=\1;/p' \\
  -e 's/\\\\def\\\\mudelapapertextheight{\([^}]*\).*$/THN=\1;/p' \\
  -e 's/\\\\def\\\\mudelapapersize{\([^}]*\).*$/fPSZ=\1;/p' \\
    "$File"`
#
if [ -z "$LATEXHF" ]
then
  LATEXHF="$fLHF"
fi
LLHF="%"
if [ -n "$LATEXHF" ]
then
  [ -f 	"$LATEXHF" ] && LLHF="\input{$LATEXHF}"
fi
#
if [ -z "$PAPERSIZE" ]
then
  PAPERSIZE="$fPSZ"
fi
if [ -n "$PAPERSIZE" ]
then
  setPaperZize
  PAPEROPT="$PAPERSIZE"
fi
#
if [ -z "$ORIENTATION" ]
then
  ORIENTATION="$fORI"
fi
if [ -n "$ORIENTATION" ]
then
  if [ -z "$PAPEROPT" ]
  then
    PAPEROPT="$ORIENTATION"
  else
    PAPEROPT="$PAPEROPT,$ORIENTATION"
  fi
fi
#
if [ -n "$PAPEROPT" ]
then
  PAPER="[$PAPEROPT]"
fi
#
if [ -z "$LANGUAGE" ]
then
  LANGUAGE="$fLNG"
fi
if [ -n "$LANGUAGE" ]
then
  LLNG="\usepackage[$LANGUAGE]{babel}"
else
  LLNG="%"
fi

#
# Find textwidth
#
if [ -n "$TWN" ]
then
  TW="$TWN"
  case "$TW" in
    *mm)
      ;;
    *cm)
      ;;
    *pt)
      ;;
    *)
      TW="${TW}pt"
      ;;
  esac
  $debug_echo "Text width = $TW"
fi
TWp=`Echo $TW | sed -e 's/\..*$//'`
PWp="$PWIDTH"
#
# Find textheight
#
if [ -n "$THN" ]
then
  TH=$THN
  case $TH in
    *mm)
      ;;
    *cm)
      ;;
    *pt)
      ;;
    *)
      TH="${TH}pt"
      ;;
  esac
  $debug_echo "Text height = $TH"
fi
THp="`echo $TH | sed -e 's/\..*$//'`"
PHp="$PHEIGHT"
if [ "$ORIENTATION" = "landscape" ]
then
  PWp="$PHEIGHT"
  PHp="$PWIDTH"
fi
HMARG="`expr '(' $PWp - $TWp ')' / 2`"pt
$debug_echo "Text left = $HMARG"
VMARG="`expr '(' $PHp - $THp ')' / 2`"pt
$debug_echo "Text top = $VMARG"
#
# Geometry: /var/lib/texmf/latex/geometry/geometry.dvi
#
#
# Write LaTeX file
#
cat << EOF > $LatF
% Creator: $IDENTIFICATION
% Automatically generated from  $IF, $NOW

\documentclass$PAPER{article}

$LLNG
\usepackage{geometry}
\usepackage[latin1]{inputenc}
%\usepackage[T1]{fontenc}
$PNUM
%\addtolength{\oddsidemargin}{-1cm}
%\addtolength{\topmargin}{-1cm}
%\setlength{\textwidth}{$TW}
%\setlength{\textheight}{$TH}
\geometry{width=$TW, left=$HMARG, height=$TH, top=$VMARG}
\input lilyponddefs
\input titledefs
$LLHF
\begin{document}
EOF
mudelaDefs
cat << EOF >> $LatF
\cmrtwenty% ugh
\makelilytitle
EOF
}
nextFile(){
cat << EOF >> $LatF
\def\theopus{}%
\def\thepiece{}%
\def\mudelaopus{}%
\def\mudelapiece{}%
EOF
mudelaDefs "\\def"
cat << EOF >> $LatF
\def\theopus{\mudelaopus}% ugh
\def\thepiece{\mudelapiece}%
\makelilypiecetitle
EOF
}
#
# Conclusion
#
endFile(){
cat << EOF >> $LatF
\vfill\hfill{\LilyIdString}
\end{document}
EOF
#
# Run LaTeX
#
latex '\nonstopmode \input '$LatF || exit 5
#
# Rename dvi file
#
if [ -f $FN.dvi ]
then
    RESULT="$BN.dvi"
    [ -n "$OUTPUTDIR" ] && RESULT="$OUTPUTDIR/$RESULT"
    
    cp "$FN.dvi" "$RESULT" || exit 5
fi
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
  OIFS="$IFS"
  IFS=':'
  x=`echo $LILYINCLUDE | sed "s!:! !g"`
  IFS="$OIFS"
  for lypath in . $x
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
    if [ -f "$lypath/$1.fly" ]
    then
       GF="$lypath/$1.fly"
       return
    fi
  done
  $debug_echo "$IDENTIFICATION: Input file \"$GF\" not found"
  echo "$NAME: Input file \"$GF\" not found"                       1>&2
  exit 2
}
#
# Loop through all files
#

LILY_OUTPUT_FILES=

for GF 
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
        TH=25.0cm
      fi
    else
      $debug_echo $IDENTIFICATION": Mudela file name not found."
      TW=15.5cm
      TH=25.0cm
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
    $debug_echo "lilypond "$LILYOPTS $IF

    $LILYPOND_EXECUTABLE $LILYOPTS $IF 2>&1  | tee $LOGFILE
    OF="`sed -n -e 's/TeX output to \([^\.]*\.tex\)\.\.\.$/\1/p' $LOGFILE`"
    $debug_echo "==> $OF"
    LILY_OUTPUT_FILES="$LILY_OUTPUT_FILES $OF"
    STATUS=`egrep -i "error|segmentation|abort" $LOGFILE`
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
    if [ ! -f "$File" ]
    then
      $debug_echo "$IDENTIFICATION: hmm, I could not find the output file $File"
      exit 4
    fi
    #
    # Is this the first file?
    #
    if [ -z "$FFile" ]
    then
      FFile="$File"
      startFile
    else
      nextFile
    fi
    cat << EOF >> $LatF
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
