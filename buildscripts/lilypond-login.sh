#!/bin/csh

# env. vars for the C-shell.

# set environment for LilyPond.  To be installed in /etc/profile.d/
setenv GS_LIB "@datadir@/ps"
setenv GS_FONTPATH "@datadir@/afm"
# bit silly. for ly2dvi, overrules compiled-in datadir...
setenv LILYPONDPREFIX "@datadir@"

# include an empty path component for the system wide includes.
if ($?MFINPUTS) then
        setenv MFINPUTS "/usr/share/lilypond/mf:${MFINPUTS}::"
else
        setenv MFINPUTS "/usr/share/lilypond/mf::"
endif
if ($?TEXINPUTS) then
        setenv TEXINPUTS "/usr/share/lilypond/tex:${TEXINPUTS}::"
else
        setenv TEXINPUTS "/usr/share/lilypond/tex::"
endif
if ($?GUILE_LOAD_PATH) then
        setenv GUILE_LOAD_PATH "/usr/share/lilypond/scm:${GUILE_LOAD_PATH}"
else
        setenv GUILE_LOAD_PATH "/usr/share/lilypond/scm"
endif


