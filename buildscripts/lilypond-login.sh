#!/bin/csh

# env. vars for the C-shell.

# set environment for LilyPond.  To be installed in /etc/profile.d/
setenv GS_LIB "@datadir@/afm"
setenv GS_FONTPATH "@datadir@/ps"
setenv LILYINCLUDE "@datadir@/ly:@datadir@/afm"

# include an empty path component for the system wide includes.
setenv MFINPUTS "@datadir@/mf:$MFINPUTS::"    # urg.
setenv TEXINPUTS "@datadir@/tex:$TEXINPUTS::"

