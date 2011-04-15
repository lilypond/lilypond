\version "2.13.36"
\header {
  lsrtags = "rhythms,expressive-marks"
  texidoc = "This code demonstrates how to use the alternative breve note
with two vertical lines on each side of the notehead instead of one line."
  doctitle = "Alternative breve notehead with double vertical lines"
}

\relative c'' {
  \time 4/2
  \override Staff.NoteHead #'style = #'altdefault
  c\breve | b\breve
}
