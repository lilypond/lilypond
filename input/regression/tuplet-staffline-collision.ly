
\version "2.17.11"
\header {

  texidoc = "Horizontal tuplet brackets are shifted vertically
to avoid staff line collisions."

}

\layout { ragged-right= ##t }

\context Voice\relative c'' {
  \tuplet 3/2 { b'4 b b }
  \tuplet 3/2 { f4 f f }
  \tuplet 3/2 { g4 g g }
  \tuplet 3/2 { a4 a a }
}

