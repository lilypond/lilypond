#!@SHELL@
# /etc/profile.d/post-lily.sh  -- Setup LilyPond

rm -f /usr/lilypond
lily=@prefix@
ln -s $lily /usr/lilypond


