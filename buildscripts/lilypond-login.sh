#!/bin/csh

# Setup TeX/LaTeX Ghostscript C-shell environment for LilyPond.
# 
# Red Hat-like systems should install this in /etc/profile.d/

# strange shell, this C
set datadir="@datadir@"

if ( $?GS_FONTPATH ) then
       setenv GS_FONTPATH "$datadir/afm:/usr/share/lilypond/pfa:$GS_FONTPATH"
 else
       setenv GS_FONTPATH "$datadir/afm:/usr/share/lilypond/pfa"
 endif
 if ( $?GS_LIB ) then
       setenv GS_LIB "$datadir/ps:$GS_LIB"
 else
       setenv GS_LIB "$datadir/ps"
 endif


# setenv LILYPONDPREFIX "$datadir"

# bit silly. for ly2dvi, overrules compiled-in datadir...
# setenv LILYPONDPREFIX "@datadir@"

# include an empty path component for the system wide includes.
if ($?MFINPUTS) then
        setenv MFINPUTS "$datadir/mf:${MFINPUTS}::"
else
        setenv MFINPUTS "$datadir/mf::"
endif
if ($?TEXINPUTS) then
        setenv TEXINPUTS "$datadir/tex:${TEXINPUTS}::"
else
        setenv TEXINPUTS "$datadir/tex::"
endif
if ($?TFMFONTS) then
        setenv TFMFONTS "$datadir/tfm:$TFMFONTS"
else
        setenv TFMFONTS "$datadir/tfm:"
endif



