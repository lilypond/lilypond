#!/bin/bash
# /etc/profile.d/lilypond-profile.z.sh  -- profile additions for Windows
# run after lilypond-profile.sh

POSIX_GS_LIB="$(cygpath -apu $GS_LIB)"
WINDOWS_GS_LIB="$(cygpath -apw $POSIX_GS_LIB)"

POSIX_GS_FONTPATH="$(cygpath -apu $GS_FONTPATH)"
WINDOWS_GS_FONTPATH="$(cygpath -apw $POSIX_GS_FONTPATH)"

POSIX_TEXINPUTS="$(cygpath -apu $TEXINPUTS)"
WINDOWS_TEXINPUTS="$(cygpath -apw $POSIX_TEXINPUTS)"

POSIX_TFMFONTS="$(cygpath -apu $TFMFONTS)"
WINDOWS_TFMFONTS="$(cygpath -apw $POSIX_TFMFONTS)"

POSIX_MFINPUTS="$(cygpath -apu $MFINPUTS)"
WINDOWS_MFINPUTS="$(cygpath -apw $POSIX_MFINPUTS)"

# we assume running dos-based TeX, ie, MiKTeX
# maybe move to /etc/profile.d/lily-tex.sh
TEXINPUTS="$WINDOWS_TEXINPUTS"
# TFMFONTS="$WINDOWS_TFMFONTS"

rm -f /usr/lilypond
# ugh
# ln -s /usr/lilypond-@TOPLEVEL_VERSION@ /usr/lilypond
lily=$(ls -utrd1 /usr/lilypond-* | tail -1)
ln -s $lily /usr/lilypond

PATH="/usr/lilypond/bin:$PATH"
PATH="$lily/bin:$PATH"

# gsv-2.7-gs-5.50
PATH="//c/gstools/gsview://c/gstools/gs:$PATH"

# gsv-3.6-gs-6.50
PATH="//c/Ghostgum/gsview://c/gstools/gs:$PATH"

# python-1.5.2
PATH="//c/Program Files/Python:$PATH"
PATH="//c/Program Files/MiKTeX/miktex:$PATH"

