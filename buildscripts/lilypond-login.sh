#!/bin/csh

# env. vars for the C-shell.

# set environment for LilyPond.  To be installed in /etc/profile.d/
setenv GS_LIB "@datadir@/afm"
setenv GS_FONTPATH "@datadir@/ps"
setenv LILYINCLUDE "@datadir@/ly:@datadir@/afm"
setenv LILYPONDPREFIX "@datadir@"

# include an empty path component for the system wide includes.
setenv MFINPUTS "@datadir@/mf:$MFINPUTS::"
setenv TEXINPUTS "@datadir@/tex:$TEXINPUTS::"
setenv GUILE_LOAD_PATH "@datadir@/scm:$GUILE_LOAD_PATH"
