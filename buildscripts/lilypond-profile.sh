#!/bin/sh

# Setup TeX/LaTeX Ghostscript environment for LilyPond.

# Red Hat-like systems should install this in /etc/profile.d/

# If run by hand or from you .profile, run as follows
#   . lilypond-profile


if [ -z "$LILYPONDPREFIX" ]; then
    datadir=`echo "@local_lilypond_datadir@" | sed 's!//!/!g'`
else
    if [ -d "$LILYPONDPREFIX/share" ]; then
	datadir=$LILYPONDPREFIX
    fi
    echo "Setting tree to $datadir"
fi

# Add the installation directory to the teTeX system tree, 
# see Documentation/misc/fontinstallation
TEXMF="{$datadir,"`kpsexpand  \\$TEXMF`"}"
export TEXMF

# For direct ps output: ps/lilyponddefs.ps
GS_LIB="$datadir/ps:"${GS_LIB:=""}
export GS_LIB

# For direct ps output fonts. Add all available TeX Type1 fonts
GS_FONTPATH=`kpsewhich -expand-path=\\$T1FONTS`:${GS_FONTPATH:=""}
export GS_FONTPATH

 	


