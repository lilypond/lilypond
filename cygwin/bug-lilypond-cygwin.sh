#!@SHELL@
# /usr/bin/bug-lilypond-cygwin.sh -- harvest relevant info for bugreport
#
# Ideas
#   * add some kpathsea tricks
#   * distribute for other platforms too
#   * failure/success tracking and indication (-> rewrite in python)

set -x

name="$(basename $0)"
# Running this in new shell may yield deceptive results
if [ "$name" = "bug-lilypond-cygwin" ]; then
    # HMM
    cat <<EOF
Please source this script, ie do:

    . /usr/bin/bug-lilypond-cygwin > bug.txt

EOF
fi

docdir=@prefix@/share/doc/lilypond-@TOPLEVEL_VERSION@
if [ ! -d $docdir ]; then
    docdir=@prefix@/doc/lilypond-@TOPLEVEL_VERSION@
fi

echo
echo Availability of executables
echo ===========================
type -p tex
type -p latex
type -p lilypond
type -p python
type -p ly2dvi

type -p tex.exe
type -p latex.exe
type -p lilypond.exe
type -p python.exe

lilypond --version
ly2dvi --version


echo
echo Test runs
echo =========
mkdir -p /tmp/lily
cd /tmp/lily
rm -f example-1.* sample2e.*
ls -l
latex \\nonstopmode \\input sample2e
cp $docdir/input/example-1.ly .
cp $docdir/input/example-2.ly .
lilypond example-1.ly
tex \\nonstopmode \\input example-1.tex
dvips -o example-1.ps example-1.dvi
ls -l
ly2dvi --verbose example-2
ls -l

echo
echo Environment settings
echo ====================
echo HOME="<$HOME>"
echo SHELL="<$SHELL>"
echo LILYPONDPREFIX="<$LILYPONDPREFIX>"
echo TEXMF="<$TEXMF>"
echo MFINPUTS="<$MFINPUTS>"
echo TFMFONTS="<$TFMFONTS>"
echo TEXINPUTS="<$TEXINPUTS>"


echo
echo System information
echo ==================
uname -a
cygcheck -s

if false; then
    cat /var/log/setup.log
    # cat /var/log/setup.log.full
fi
