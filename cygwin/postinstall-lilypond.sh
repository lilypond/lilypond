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

regtool set '/root/LilyPond/shell/open/command/' $ROOT'\bin\bash.exe --login -c '"'"'/usr/bin/lily-wins "%1"'"'"
regtool add '/root/LilyPond/shell/edit'
regtool set '/root/LilyPond/shell/edit/' '&Edit source in Notepad ...'
regtool add '/root/LilyPond/shell/edit/command'
regtool set '/root/LilyPond/shell/edit/command/' '%SystemRoot%\system32\notepad.exe %1'
regtool add '/root/LilyPond/shell/generate'
regtool set '/root/LilyPond/shell/generate/' '&Generate PDF ...'
regtool add '/root/LilyPond/shell/generate/command'
regtool set '/root/LilyPond/shell/generate/command/' $ROOT'\bin\bash.exe --login -c '"'"'/usr/bin/lily-wins "%1"'"'"

# FIXME: move to new postinstall-lilypond-doc.sh
[ -d /usr/share/info/lilypond ] && (cd /usr/share/info/lilypond && ln -sf ../../doc/lilypond/Documentation/user/out-www/*png .)
