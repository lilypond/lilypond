#!/bin/bash -e
#
# postinst script for the Debian GNU/Linux lilypond package
#   by Anthony Fok <foka@gpu.srv.ualberta.ca>
#   This is free software; see the GNU General Public Licence
#   version 2 or later for copying conditions.  There is NO warranty.
#   Last modified:  Wed,  6 Aug 1997 13:42:45 -0600

std_TEXMF=/usr/lib/texmf

if [ "`which kpsetool`" ]; then
    TEXMF=`kpsetool -v '$TEXMF'`
fi
: ${TEXMF:=$std_TEXMF}

if [ "$TEXMF" = "$std_TEXMF" -a "`which texhash`" -a -e $TEXMF/ls-R ]; then
    texhash
fi
