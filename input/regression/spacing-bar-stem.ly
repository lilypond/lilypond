
\version "2.10.0"
\header {
  texidoc = "Downstem notes following a barline are
printed with some extra space. This is an optical correction similar
to juxtaposed stems.

Accidentals after the barline get some space as well.
"
}

\layout { ragged-right = ##t}

\relative c''
{

  %%\override Staff.StaffSpacing  #'stem-spacing-correction = #10
  %%\override Staff.NoteSpacing  #'stem-spacing-correction = #10

  \time 1/4 \stemDown c4 \stemUp c4
  \stemDown c4 \stemUp c4
  \stemDown f c,4  c'4 cis4 \stemUp c4
}



