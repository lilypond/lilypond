#!/bin/csh

# Setup TeX/LaTeX Ghostscript C-shell environment for LilyPond.
# 
# Red Hat-like systems should install this in /etc/profile.d/

# If run by hand or from you .login, run as follows
#   source lilypond-profile



# strange shell, this C
set datadir="@datadir@"

 if ( $?GS_LIB ) then
       setenv GS_LIB "$datadir/ps:$GS_LIB"
 else
       setenv GS_LIB "$datadir/ps"
 endif


# setenv LILYPONDPREFIX "$datadir"

# bit silly. for ly2dvi, overrules compiled-in datadir...
# setenv LILYPONDPREFIX "@datadir@"

# Add the installation directory to the teTeX system tree, 
# see Documentation/misc/fontinstallation

set noglob
setenv TEXMF "{$datadir,"`kpsexpand  \$TEXMF`"}"
unset noglob

# Add all available TeX Type1 fonts (including Feta) to Ghostscript:
if ( $?GS_FONTPATH ) then
       setenv GS_FONTPATH `kpsewhich -expand-path=\$T1FONTS`:$GS_FONTPATH"
 else
       setenv GS_FONTPATH `kpsewhich -expand-path=\$T1FONTS`
 endif


