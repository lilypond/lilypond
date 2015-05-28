
\version "2.19.21"
\header {

  texidoc = "Horizontal tuplet brackets are shifted vertically
to avoid staff line collisions."

}

\layout { ragged-right= ##t }

\context Voice\relative {
  \tuplet 3/2 { b''4 b b }
  \tuplet 3/2 { f4 f f }
  \tuplet 3/2 { g4 g g }
  \tuplet 3/2 { a4 a a }
}

