#!/bin/sh


# set environment for LilyPond.  To be installed in /etc/profile.d/
GS_FONTPATH="@datadir@/afm:@datadir@/pfa"

# bit silly. for ly2dvi, overrules compiled-in datadir...
LILYPONDPREFIX="@datadir@"

# include an empty path component for the system wide includes.
MFINPUTS="@datadir@/mf:"${MFINPUTS:=":"}
TEXINPUTS="@datadir@/tex:@datadir@/ps:"${TEXINPUTS:=":"}


## gs_lib ??? 
export LILYINCLUDE LILYPONDPREFIX MFINPUTS TEXINPUTS GS_LIB GS_FONTPATH

 	


