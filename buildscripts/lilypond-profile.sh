#!/bin/sh


# set environment for LilyPond.  To be installed in /etc/profile.d/
GS_LIB="@datadir@/afm"
GS_FONTPATH="@datadir@/ps"
LILYINCLUDE="@datadir@/ly:@datadir@/afm"

# include an empty path component for the system wide includes.
MFINPUTS="@datadir@/mf:"${MFINPUTS:=":"}
TEXINPUTS="@datadir@/tex:"${TEXINPUTS:=":"}

export LILYINCLUDE MFINPUTS TEXINPUTS GS_LIB GS_FONTPATH

# echo  $LILYINCLUDE $MFINPUTS $TEXINPUTS $GS_LIB $GS_FONTPATH


