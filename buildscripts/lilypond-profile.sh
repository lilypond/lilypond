#!/bin/sh

# Setup TeX/LaTeX Ghostscript environment for LilyPond.

# Red Hat-like systems should install this in /etc/profile.d/

# If run by hand or from you .profile, run as follows
#   . lilypond-profile


if [ -z "$LILYPONDPREFIX" ]; then
    datadir=`echo "@local_lilypond_datadir@" | sed 's!//!/!g'`
else
    if [ -d "$LILYPONDPREFIX/fonts" ]; then
	datadir=$LILYPONDPREFIX
    else
	eval `cat $LILYPONDPREFIX/VERSION`
	FULLVERSION="$MAJOR_VERSION.$MINOR_VERSION.$PATCH_LEVEL"
	if [ "" != "$MY_PATCH_LEVEL" ]; then
	    FULLVERSION="$FULLVERSION.$MY_PATCH_LEVEL"
	fi 
	
	datadir="$LILYPONDPREFIX/share/lilypond/$FULLVERSION"
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

 	


