#!@SHELL@
# /etc/profile.d/post-lily.sh  -- Setup LilyPond

touch /tmp/.lilypond-install
rm $(find /var/lib/texmf /var/spool/texmf -name 'feta*pk' -or -name 'feta*tfm' -or -name 'parmesan*pk' -or -name 'parmesan*tfm')
rm -f /tmp/.lilypond-install



