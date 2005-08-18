
\version "2.6.0"
\header {

  texidoc = "Horizontal tuplet brackets are shifted vertically
to avoid staff line collisions."

}

\layout { raggedright= ##t }

\context Voice\relative c'' {
  \times 2/3 { b'4 b b }
  \times 2/3 { f4 f f }
  \times 2/3 { g4 g g }
  \times 2/3 { a4 a a }
}

