#!@SHELL@
# /etc/profile.d/gs-profile.sh -- Check for GhostScript

gs550="/usr/windows/gstools/gs5.50"
gs650="/usr/windows/gs/gs6.50"

# Maybe read registry, but that may be hairy?
# 
# $ regtool get \\HKLM\\Software\\CLASSES\\psfile\\shell\\open\\command\\
# "C:\GSTOOLS\GSVIEW\gsview32.exe" "%1"

## we set GS_LIB although the registry keys have been set.

if [ -e "$gs550/gswin32.exe" ]; then
	PATH="$gs550:$PATH"
	GS_LIB="$gs550/lib:"${GS_LIB:=""}
fi

if [ -e "$gs650/bin/gswin32.exe" ]; then
	PATH="$gs650/bin:$PATH"
	GS_LIB="$gs650/lib:"${GS_LIB:=""}
fi
