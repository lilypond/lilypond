#!/bin/csh

# Setup TeX/LaTeX Ghostscript C-shell environment for LilyPond.
# 
# Red Hat-like systems should install this in /etc/profile.d/

setenv GS_FONTPATH "@datadir@/afm:@datadir@/pfa:$GS_FONTPATH"
setenv GS_LIB "@datadir@/ps:$GS_LIB"

# bit silly. for ly2dvi, overrules compiled-in datadir...
# setenv LILYPONDPREFIX "@datadir@"

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
if ($?TFMFONTS) then
        setenv TFMFONTS "@datadir@/tfm:$TFMFONTS"
else
        setenv TFMFONTS "@datadir@/tfm:"
endif



