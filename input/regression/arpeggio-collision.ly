\version "2.17.17"

\header  {
texidoc = "Arpeggio stays clear of accidentals and flipped note heads."
}
\layout {
  ragged-right = ##t
  \context{
    \Staff
    connectArpeggios = ##t
    \consists "Span_arpeggio_engraver"
  }
}



\transpose c c' {
  <fis'' g d a>\arpeggio
  <fis, g d a>\arpeggio
  <fis'' g d a>\arpeggio
  << { <e' a>\arpeggio } \\ { <g cis>\arpeggio } >>
}
