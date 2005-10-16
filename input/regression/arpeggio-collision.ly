\version "2.7.13"

\header  {
texidoc = "Arpeggio stays clear of accidentals and flipped note heads."
}
\layout {
  raggedright = ##t
  \context{
    \Staff
    connectArpeggios = ##t
  }
}



\transpose c c' {
  <fis'' g d a>\arpeggio
  <fis, g d a>\arpeggio
  <fis'' g d a>\arpeggio
}
