#!/bin/bash
if echo | python -; then
	cat <<EOF
Python not found.  Install python-1.5.2 from www.python.org.
Make sure python.exe in in your $PATH
EOF
fi
python `cygpath -w /usr/lilypond/bin/ly2dvi.py` $*
