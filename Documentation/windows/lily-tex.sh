#!/bin/bash
# /etc/profile.d/lily-tex.sh -- Check for TeX or try to setup MiKTeX

texmf='//c/Program Files/MiKTeX'
texmf_bin='//c/Program Files/MiKTeX/miktex'

if [ ! -e /usr/share/texmf ]; then
	echo /usr/share/texmf not found
	if [ -e "$texmf" ]; then
		ln -s "$texmf" /usr/share/texmf
		echo MiKTeX found: linked to "$texmf"
	else
		echo setup /usr/share/texmf yourself
	fi
fi
if type -p tex; then
	echo no TeX in PATH
	if [ -e "$texmf_bin" ]; then
		echo MiKTeX found: added "$texmf_bin" to PATH
		PATH=`cygpath -u "$texmf/miktex"`:"$PATH"
	fi
fi

