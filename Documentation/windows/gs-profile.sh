#!@SHELL@
# /etc/profile.d/gs-profile.sh -- Check for GhostScript

gs550="/usr/windows/gstools/gs5.50"
gs650="/usr/windows/gs/gs6.50/bin"

# Maybe read registry, but that may be hairy?
# 
# $ regtool get \\HKLM\\Software\\CLASSES\\psfile\\shell\\open\\command\\
# "C:\GSTOOLS\GSVIEW\gsview32.exe" "%1"

if [ -e "$gs550/gswin32.exe" ]; then
	PATH="$gs550:$PATH"
fi

if [ -e "$gs650/gswin32.exe" ]; then
	PATH="$gs650:$PATH"
fi
