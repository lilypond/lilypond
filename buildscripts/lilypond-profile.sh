#!/bin/sh

# Setup TeX/LaTeX Ghostscript environment for LilyPond.

# Red Hat-like systems should install this in /etc/profile.d/

# This is a bit of a kludge.  Ideally, lilypond's tex, afm, pfa, ps
# directories should be installed into their location in the texmf/gs
# trees, rather than messing around with environment variables (eg,
# see Debian or SuSE package).

datadir="@datadir@"

# For direct ps output fonts
GS_FONTPATH="$datadir/afm:$datadir/pfa"

# For direct ps output: ps/lilyponddefs.ps
GS_LIB="$datadir/ps"

# bit silly. for ly2dvi, overrules compiled-in datadir...
# Better comment this out.  Compiled-in datadir serves exactly the
# same purpose, but is more likely to be correct (think multiple
# versions of lilypond).
# LILYPONDPREFIX="$datadir"

# include an empty path component for the system wide includes.
MFINPUTS="$datadir/mf:"${MFINPUTS:=":"}
TEXINPUTS="$datadir/tex:$datadir/ps:"${TEXINPUTS:=":"}
TFMFONTS="$datadir/tfm:"${TFMFONTS:=":"}
LILYPONDPREFIX="$datadir"

export MFINPUTS TEXINPUTS TFMFONTS GS_LIB GS_FONTPATH LILYPONDPREFIX

 	


