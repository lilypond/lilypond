\version "2.19.28"

\header {
  texidoc = "@code{\\once \\revert} can be used for reverting a property
once rather than permanently."
}

\layout {
  ragged-right = ##t
}

\relative {
  c'4-"b" d-"b"
  \override NoteHead.color = #red
  e4-"r" f-"r" |
  \once \override NoteHead.color = #green
  g4-"g" a-"r"
  \once \revert NoteHead.color
  b-"b" c-"r" |
  \temporary \override NoteHead.color = #yellow
  g-"y" e-"y"
  \once \revert NoteHead.color
  d-"r" c-"y" |
  \revert NoteHead.color
  d-"r" e-"r"
  \once \revert NoteHead.color
  f-"b" d-"r" |
  \revert NoteHead.color
  c1-"b"
}
