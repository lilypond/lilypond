#!/bin/sh


# set environment for LilyPond.  To be installed in /etc/profile.d/
GS_LIB="@datadir@/afm"
GS_FONTPATH="@datadir@/ps"
LILYINCLUDE="@datadir@/ly:@datadir@/afm"
# bit silly. for ly2dvi, overrules compiled-in datadir...
LILYPONDPREFIX="@datadir@"

# include an empty path component for the system wide includes.
MFINPUTS="@datadir@/mf:"${MFINPUTS:=":"}
TEXINPUTS="@datadir@/tex:@datadir@/ps:"${TEXINPUTS:=":"}

GUILE_LOAD_PATH="@datadir@/scm:$GUILE_LOAD_PATH"

export GUILE_LOAD_PATH LILYINCLUDE LILYPONDPREFIX MFINPUTS TEXINPUTS GS_LIB GS_FONTPATH

 	
# echo  $LILYINCLUDE $MFINPUTS $TEXINPUTS $GS_LIB $GS_FONTPATH


