#!@SHELL@
# /etc/profile.d/post-python.sh -- Check for Python

# Educated guess in case we have no regtool
a="//c/Program Files/Python"

# Registry entry
reg="$(regtool -q get 'HKLM\Software\Python\PythonCore\1.5\InstallPath\')"
b="$(cygpath -u ""$reg"")"

# Where we installed it
c="/usr/windows/Python"

for i in "$a" "$b" "$c"; do
	if [ -d "$i" ]; then
		python="$i"
	fi
done

PATH="$python:$PATH"
