#!@SHELL@
# /etc/profile.d/lily-miktex.sh -- Check for MiKTeX


# Educated guess in case we have no regtool
a="//c/Program Files/MiKTeX"

# Registry entry
reg="$(regtool -q get 'HKLM\Software\MiK\MiKTeX\CurrentVersion\MiKTeX\Install Root\')"
b="$(cygpath -u ""$reg"")"

# Where we installed it
c=texmf="/usr/windows/MiKTeX"

for i in "$a" "$b" "$c"; do
	if [ -d "$i" ]; then
		texmf="$i"
	fi
done

PATH="$texmf/miktex/bin:$PATH"

