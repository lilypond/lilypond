#!@SHELL@
# @name@.sh -- @name@ wrapper for Windows

if echo | python - >/dev/null 2>&1; then
	echo
else
	cat <<EOF
Python not found.  Install python-1.5.2 from www.python.org.
Make sure python.exe in in your PATH
EOF
	exit 1
fi
python "$(cygpath -w ""@prefix@/bin/@name@.py"")" $*
