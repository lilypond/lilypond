#!/bin/sh

# Setup TeX/LaTeX Ghostscript environment for LilyPond.

# Red Hat-like systems should install this in /etc/profile.d/

# If run by hand or from you .profile, run as follows
#   . /PATH/TO/lilypond-profile





# In ZSH, $0 is set to the script name, regardless of whether sourced
# or run in a subshell.
if [ -n "$ZSH_NAME" ] ; then
    echo "Make sure that this script is sourced, ie. run as "
    echo 
    echo "   . lilypond-profile"
    echo ""
    echo "Continuing anyway ..."
    return 2
fi

if [ -z "$ZSH_NAME" -a `basename "$0"` = "lilypond-profile" ] ; then 
    cat >/dev/stderr <<EOF
    
Error: This script cannot be run in a subshell; it MUST be sourced.


EXAMPLE 1: One time use 

	* Do

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

    return 2
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
	if [ -z `echo $TEXMF | grep "$datadir"` ]; then
		TEXMF="{$datadir,"`kpsexpand  \\$TEXMF`"}"
		export TEXMF
	fi
fi
 	


