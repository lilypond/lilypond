\version "2.27.5"

\header
{
  texidoc = "When staff lines are hidden, bar lines extend to the outer edges
of the hidden staff lines.  In this case, the bar line should span the orange
background exactly."
}

\markup \scale #'(5 . 5) \score {
  \layout {
    indent = 0
  }
  \new Staff \with {
    \remove Clef_engraver
    \remove Time_signature_engraver
    \hide StaffSymbol
    \override StaffSymbol.thickness = 9
    %% Counteract thickening the stem to avoid raising questions.
    \override Stem.thickness = #1/9
  } {
    \staffHighlight #(universal-color 'orange)
    f'8 \bar "|.|" e''8
  }
}
