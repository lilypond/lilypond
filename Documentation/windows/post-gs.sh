#!@SHELL@
# /etc/postinstall/post-gs.sh -- Install GS and GSView


gs550="/usr/windows/gstools/gs5.50"
gs650="/usr/windows/gs/gs6.50"

gsview26="/usr/windows/gstools/gsview"
gsview36="/usr/windows/Ghostgum/GSview"

# maybe set this too, but how (what type is value, a list?)
# $ regtool get \\HKLM\\Software\\CLASSES\\psfile\\shell\\open\\command\\
# "C:\GSTOOLS\GSVIEW\gsview32.exe" "%1"

# gsview needs gs register entries, so it seems

if [ -e "$gs550/gswin32.exe" ]; then
	regtool -s set 'HKLM\Software\Aladdin Ghostscript\5.50\GS_LIB' \
		"c:\cygwin\windows\gstools\gs5.50;c:\cygwin\windows\gstools\gs5.50\fonts"
	regtool -s set 'HKLM\Software\Aladdin Ghostscript\5.50\GS_DLL' \
		"c:\cygwin\windows\gstools\gs5.50\\gsdll32.dll"
fi

if [ -e "$gs650/gswin32.exe" ]; then
	regtool -s set 'HKLM\Software\AFPL Ghostscript\6.50\GS_DLL' \
		"C:\cygwin\windows\gs\gs6.50\bin\gsdll32.dll"
	regtool -s set 'HKLM\Software\AFPL Ghostscript\6.50\GS_LIB' \
		"C:\cygwin\windows\gs\gs6.50\lib;C:\cygwin\windows\gs\fonts"
fi

# nothing to be done
if [ -e "$gsview26/gsview32.exe" ]; then
	true
fi

if [ -e "$gsview36/gsview32.exe" ]; then
	regtool -s set 'HKLM\Software\Ghostgum\GSview\3.6' \
		"C:\cygwin\windows\Ghostgum"
fi

# What's in the registry

#  regtool -v list HKLM\Software\Aladdin Ghostscript\5.50
#  GS_LIB = "c:\cygwin\windows\gstools\gs5.50;c:\cygwin\windows\gstools\gs5.50\fonts"
#  GS_DLL = "c:\cygwin\windows\gstools\gs5.50\\gsdll32.dll"

#  regtool -v list HKLM\Software\Ghostgum\GSview
#  3.6 = "C:\cygwin\windows\Ghostgum"

#  regtool -v list HKLM\Software\AFPL Ghostscript\6.50
#  GS_DLL = "C:\cygwin\windows\gs\gs6.50\bin\gsdll32.dll"
#  GS_LIB = "C:\cygwin\windows\gs\gs6.50\lib;C:\cygwin\windows\gs\fonts"


