#!@SHELL@
# /etc/profile.d/gsview-profile.sh  Check for GhostView

gsview26="/usr/windows/gstools/gsview"
gsview36="/usr/windows/Ghostgum/GSview"

# Maybe read registry, but that may be hairy?
# 
# $ regtool get \\HKLM\\Software\\CLASSES\\psfile\\shell\\open\\command\\
# "C:\GSTOOLS\GSVIEW\gsview32.exe" "%1"

if [ -e "$gsview26/gsview32.exe" ]; then
	PATH="$gsview26:$PATH"
fi

if [ -e "$gsview36/gsview32.exe" ]; then
	PATH="$gsview36:$PATH"
fi

