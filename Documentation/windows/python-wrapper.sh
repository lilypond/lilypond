#!@SHELL@
# @name@.sh -- @name@ wrapper for Windows

if echo | python - >/dev/null 2>&1; then
	echo
else
	cat <<EOF
Python not found.  Install python-2.1-1 from http://cygwin.com
Make sure python.exe in in your PATH
EOF
	exit 1
fi
python "@prefix@/bin/@name@.py" $*
