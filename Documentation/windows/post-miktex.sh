#!@SHELL@
# /etc/profile.d/post-miktex.sh -- Setup MiKTeX


# Educated guess in case we have no regtool
a="//c/Program Files/MiKTeX"

# Registry entry
reg="$(regtool -q get 'HKLM\Software\MiK\MiKTeX\CurrentVersion\MiKTeX\Install Root\')"
b="$(cygpath -u ""$reg"")"

# Where we installed it
c="/usr/windows/MiKTeX"

for i in "$a" "$b" "$c"; do
	if [ -d "$i" ]; then
		texmf="$i"
	fi
done

rm -f /usr/share/texmf
ln -s "$texmf" /usr/share/texmf

# What's in the registry
# $ regtool -s set 'HKLM\Software\MiK\MiKTeX\CurrentVersion\MiKTeX\Install Root\' "C:\cygwin\usr\windows\MiKTeX"

# regtool -v list HKLM\Software\MiK\MiKTeX\CurrentVersion\MiKTeX
# TEXMF Root Directories = "C:\cygwin\usr\windows\miktex\spool\texmf;C:\cygwin\windows\MiKTeX"
# Install Root = "C:\cygwin\usr\windows\MiKTeX"

