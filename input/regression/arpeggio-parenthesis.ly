
\header {

  texidoc = "There is a variant of the arpeggio sign that uses a
  `vertical slur' instead of the wiggle."

}

\version "2.12.0"

\relative c' {
  \arpeggioParenthesis

  % Note: does not work for cross staff arpeggios.
  \override Arpeggio #'X-extent = #ly:grob::stencil-width
  <c g' c>2\arpeggio ~ c
}
