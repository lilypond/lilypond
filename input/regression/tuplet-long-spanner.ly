\version "2.25.8"

\header {
  texidoc = "Overlong tuplet span specifications are reduced
to the actual length."
}

\layout { ragged-right = ##t }

\relative {
  \set subdivideBeams = ##t
  \tuplet 3/2 4 { g16 a b } c8 d e f2 |
  \tuplet 3/2 4
  { d16 e f g a b  c,16 d e f g a
    b, c d }
  \tuplet 3/2 { e16 f8 } g4 |
}
