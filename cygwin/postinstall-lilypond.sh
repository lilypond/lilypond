#!@SHELL@
# /etc/postinstall/lilypond.sh  -- Setup LilyPond

# register .ly
ROOT=$(cygpath -w /)
[ -z "$ROOT" ] && ROOT=$(regtool get '/machine/Software/Cygnus Solutions/Cygwin/mounts v2/\\\//native')
[ -z "$ROOT" ] && ROOT=$(cat '/proc/registry/HKEY_LOCAL_MACHINE/Software/Cygnus Solutions/Cygwin/mounts v2/\\\//native')
[ -z "$ROOT" ] && ROOT='c:\cygwin'

regtool add '/root/.ly'
regtool set '/root/.ly/' 'LilyPond'
regtool set '/root/.ly/Content Type' 'text/lilypond-source'
regtool add '/root/LilyPond'
regtool set '/root/LilyPond/' 'LilyPond source'
regtool add '/root/LilyPond/shell'
regtool add '/root/LilyPond/shell/open'
regtool add '/root/LilyPond/shell/open/command'

# old ideas - discarded for now
# regtool set '/root/LilyPond/shell/open/command/' $ROOT'\bin\python /usr/bin/lily-wins %1'
# invoking python directly does not work
# regtool set '/root/LilyPond/shell/open/command/' $ROOT'\bin\run-lily-wins.bat %1'
# we now use generated run-lily-wins.bat in /
# regtool set '/root/LilyPond/shell/open/command/' $ROOT'\run-lily-wins.bat %1'
# the direct bash route:

# TODO: should 'open' run LilyPond?
#       should 'open' also start PDF viewer?
regtool set '/root/LilyPond/shell/open/command/' $ROOT'\bin\bash.exe --login -c '"'"'/usr/bin/lily-wins "%1"'"'"
regtool add '/root/LilyPond/shell/edit'
regtool set '/root/LilyPond/shell/edit/' '&Edit source in Notepad ...'
regtool add '/root/LilyPond/shell/edit/command'
regtool set '/root/LilyPond/shell/edit/command/' '%SystemRoot%\system32\notepad.exe %1'
regtool add '/root/LilyPond/shell/generate'
regtool set '/root/LilyPond/shell/generate/' '&Generate PDF ...'
regtool add '/root/LilyPond/shell/generate/command'
# regtool set '/root/LilyPond/shell/generate/command/' $ROOT'\bin\python /usr/bin/lily-wins %1'
regtool set '/root/LilyPond/shell/generate/command/' $ROOT'\bin\bash.exe --login -c '"'"'/usr/bin/lily-wins "%1"'"'"

# static run-lily-wins.bat, does not work.
# @echo off
# rem run-lily-wins.bat - Invoke /usr/bin/lily-wins from explorer
# rem cd %~dp0%
# rem bash.exe --login -c "/usr/bin/lily-wins '%1%'"
# rem only works in Windows NT
# rem %~dp0~bin\bash.exe --login -c "/usr/bin/lily-wins '%1%'"

# generate run-lily-wins.bat - not necessary: direct bash route
# how to handle \r\n endings? text/bin mode?
#cat > /run-lily-wins.bat <<EOF
#@echo off
#rem run-lily-wins.bat - Invoke /usr/bin/lily-wins from explorer
#$ROOT\\bin\\bash.exe --login -c "/usr/bin/lily-wins '%1%'"
#EOF

# cleanup old fonts
touch /tmp/.lilypond-install
rm $(find /var/lib/texmf /var/spool/texmf /var/cache/fonts -name 'feta*pk' -or -name 'feta*tfm' -or -name 'parmesan*pk' -or -name 'parmesan*tfm')
rm -f /tmp/.lilypond-install
