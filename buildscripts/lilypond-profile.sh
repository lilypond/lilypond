#!/bin/sh

# Setup TeX/LaTeX Ghostscript environment for LilyPond.

# Red Hat-like systems should install this in /etc/profile.d/

# If run by hand or from you .profile, run as follows
#   . lilypond-profile

datadir=`echo "@datadir@" | sed 's!//!/!g'`

# For direct ps output fonts
GS_FONTPATH="$datadir/fonts/afm:$datadir/fonts/type1:"${GS_FONTPATH:=""}

# For direct ps output: ps/lilyponddefs.ps
GS_LIB="$datadir/ps:"${GS_LIB:=""}

# bit silly. for ly2dvi, overrules compiled-in datadir...
# Better comment this out.  Compiled-in datadir serves exactly the
# same purpose, but is more likely to be correct (think multiple
# versions of lilypond).
# LILYPONDPREFIX="$datadir"

# Add the installation directory to the teTeX system tree, 
# see Documentation/misc/fontinstallation
TEXMF="{$DATADIR,"`kpsexpand  \\$TEXMF`"}"

# LILYPONDPREFIX="$datadir"
# export LILYPONDPREFIX

export GS_LIB GS_FONTPATH TEXMF

 	


