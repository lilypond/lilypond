
\version "2.3.16"
\header {
texidoc = "Downstem notes following a barline are
printed with some extra space. This is an optical correction similar
to juxtaposed stems.

Accidentals after the barline get some space as well.
"
}

sd = \override Stem  #'direction = #-1
su = \override Stem  #'direction = #1
\score { \relative c''
{

%\override Staff.StaffSpacing  #'stem-spacing-correction = #10
%\override Staff.NoteSpacing  #'stem-spacing-correction = #10

\time 1/4 \sd c4 \su c4
\sd c4 \su c4
\sd f c,4  c'4 cis4 \stemUp c4
}
\paper { raggedright = ##t}
}

