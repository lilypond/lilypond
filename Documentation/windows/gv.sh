#!/bin/bash
if echo | gsview32 -; then
	cat <<EOF
GSView not found.  Install gsview from 
http://cm.bell-labs.com/who/wim/ghost/gsv26550.exe
Make sure gsview32.exe in in your $PATH
EOF
fi
gsview32 `cygpath -w $1`
