#!@SHELL@
# /etc/profile.d/post-lily.sh  -- Setup LilyPond

# touch /tmp/.lilypond-install  -- from redhat.spec, why?
rm `find /var/lib/texmf -name 'feta*pk' -or -name 'feta*tfm' -or -name 'parmesan*pk' -or -name 'parmesan*tfm' -print'`
# rm /tmp/.lilypond-install

rm -f /usr/lilypond
lily=@prefix@
ln -s $lily /usr/lilypond


