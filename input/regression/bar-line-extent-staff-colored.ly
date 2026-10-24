\version "2.27.5"

\header
{
  texidoc = "When bar lines and staff lines are not the same color, bar lines
extends to the outer edges of the staff lines.  The staff lines in this test
are thickened to exaggerate differences."
}

\markup \scale #'(5 . 5) \score {
  \layout {
    indent = 0
  }
  \new Staff \with {
    \remove Clef_engraver
    \remove Time_signature_engraver
    \override StaffSymbol.color = #(universal-color 'vermillion)
    \override StaffSymbol.thickness = 5
    %% Counteract thickening the stem to avoid raising questions.
    \override Stem.thickness = #1/5
  } {
    f'8 \bar "|.|" e''8
  }
}
