#!/bin/sh

# Setup TeX/LaTeX Ghostscript environment for LilyPond.

# Red Hat-like systems should install this in /etc/profile.d/

# If run by hand or from you .profile, run as follows
#   . /PATH/TO/lilypond-profile

echo $0
# workaround for ZSH posix $0-problem
[ -n "$ZSH_NAME" ] && setopt nofunctionargzero

if [ `basename "$0"` = "lilypond-profile" ] ; then
    cat >/dev/stderr <<EOF
    
Error: This script cannot be run in a subshell; it MUST be sourced.


EXAMPLE 1: One time use 

	Do

	    . $0



EXAMPLE 2: Install for self

	* Do

	    cp lilypond-profile $HOME/bin/

	* Add

	    .  $HOME/bin/lilypond-profile

	to either $HOME/.profile or $HOME/.bash_profile, 

	* Logout.


EXAMPLE 3: Install for all users.

	* Do

	     mv lilypond-profile /etc/profile.d/lilypond.sh

	* Logout



EOF


## If the message above is printed erroneously,
## the following commands  kills the current terminal/shell.
## hence the ZSH  test above.

    exit 2 
else

	if [ -z "$LILYPONDPREFIX" ]; then
	    datadir=`echo "@local_lilypond_datadir@" | sed 's!//!/!g'`
	else
	    if [ -d "$LILYPONDPREFIX/share" ]; then
		datadir=$LILYPONDPREFIX/share/lilypond/
	    fi
	    echo "Setting tree to $datadir"
	fi

	# Add the installation directory to the teTeX system tree, 
	# see Documentation/misc/fontinstallation
	TEXMF="{$datadir,"`kpsexpand  \\$TEXMF`"}"
	export TEXMF

	# For direct ps output: ps/lilyponddefs.ps
	## GS_LIB="$datadir/ps:"${GS_LIB:=""}
	## export GS_LIB

	# For direct ps output fonts. Add all available TeX Type1 fonts
	## GS_FONTPATH=`kpsewhich -expand-path=\\$T1FONTS`:${GS_FONTPATH:=""}
	## export GS_FONTPATH

fi
 	


