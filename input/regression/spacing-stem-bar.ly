\header {

texidoc = "Upstem notes before a barline are printed with some extra
space. This is an optical correction similar to juxtaposed stems.
"

}

sd = \property Voice.Stem \set #'direction = #-1
su = \property Voice.Stem \set #'direction = #1
\score { \notes\relative e'
{

%\property Staff.StaffSpacing \override #'stem-spacing-correction = #0.5
%\property Staff.NoteSpacing \override #'stem-spacing-correction = #0.5

\time 3/8
\su
e8 e e
f f f
a a a
c c c
e e e
}
\paper { linewidth = -1. }
}
