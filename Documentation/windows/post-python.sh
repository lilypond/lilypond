#!@SHELL@
# /etc/profile.d/post-python.sh -- Setup Python

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

# What's in the registry

#  regtool -v list HKLM\Software\Python\PythonCore\1.5
#  InstallPath\ ()
#  PythonPath\ ()
#  Dll\ ()
#  Modules\ ()

#  regtool -v list HKLM\Software\Python\PythonCore\1.5\InstallPath
#  InstallGroup\ ()
#   = "C:\cygwin\usr\windows\Python"

#  regtool -v list HKLM\Software\Python\PythonCore\1.5\PythonPath
#   = "C:\cygwin\usr\windows\Python\Lib\plat-win;C:\cygwin\usr\windows\Python\Lib;C:\cygwin\usr\windows\Python\DLLs;C:\cygwin\usr\windows\Python\Lib\lib-tk"

#  regtool -v list HKLM\Software\Python\PythonCore\1.5\Dll
#   = "C:\WINDOWS\SYSTEM\Python15.dll"

#  regtool -v list HKLM\Software\Python\PythonCore\1.5\Modules\
#   = ""


