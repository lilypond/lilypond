#!/bin/sh

# Setup TeX/LaTeX Ghostscript environment for LilyPond.

# Red Hat-like systems should install this in /etc/profile.d/

# If run by hand or from you .profile, run as follows
#   . lilypond-profile

datadir=`echo "@datadir@" | sed 's!//!/!g'`

# For direct ps output: ps/lilyponddefs.ps
GS_LIB="$datadir/ps:"${GS_LIB:=""}

# bit silly. for ly2dvi, overrules compiled-in datadir...
# Better comment this out.  Compiled-in datadir serves exactly the
# same purpose, but is more likely to be correct (think multiple
# versions of lilypond).
# LILYPONDPREFIX="$datadir"

# Add the installation directory to the teTeX system tree, 
# see Documentation/misc/fontinstallation
TEXMF="{$datadir,"`kpsexpand  \\$TEXMF`"}"

# LILYPONDPREFIX="$datadir"
# export LILYPONDPREFIX

# For direct ps output fonts. Add all available TeX Type1 fonts
GS_FONTPATH=`kpsewhich -expand-path=\$T1FONTS`:${GS_FONTPATH:=""}



export GS_LIB GS_FONTPATH TEXMF

 	


