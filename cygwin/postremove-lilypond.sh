#!@SHELL@
# /etc/postremove/lilypond.sh -- Remove all traces of LilyPond

regtool remove '/root/.ly'
regtool remove '/root/LilyPond/shell/open/command'
regtool remove '/root/LilyPond/shell/open'
regtool remove '/root/LilyPond/shell/edit/command'
regtool remove '/root/LilyPond/shell/edit'
regtool remove '/root/LilyPond/shell/generate/command'
regtool remove '/root/LilyPond/shell/generate'
regtool remove '/root/LilyPond/shell'
regtool remove '/root/LilyPond'

# cleanup old fonts
touch /tmp/.lilypond-install
rm $(find /var/lib/texmf /var/spool/texmf /var/cache/fonts -name 'feta*pk' -or -name 'feta*tfm' -or -name 'parmesan*pk' -or -name 'parmesan*tfm')
rm -f /tmp/.lilypond-install
