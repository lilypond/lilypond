\version "1.5.68"
\header {
texidoc = "Downstem notes following a barline are
printed with some extra space. This is an optical correction similar
to juxtaposed stems.

Accidentals after the barline get some space as well.
"
}

sd = \property Voice.Stem \set #'direction = #-1
su = \property Voice.Stem \set #'direction = #1
\score { \notes\relative c''
{

%\property Staff.StaffSpacing \override #'stem-spacing-correction = #10
%\property Staff.NoteSpacing \override #'stem-spacing-correction = #10

\time 1/4 \sd c4 \su c4
\sd c4 \su c4
\sd f c,4  c'4 cis4 \stemUp c4
}
\paper { linewidth = -1. }
}
