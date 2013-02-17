
\header {

  texidoc = "For slope calculations, stemlets are treated
as invisible stems."

}

\version "2.17.11"

\layout {
  ragged-right = ##t
  indent = #0
}

\relative c' {
  \tuplet 3/2 {r8[ c' g']}
  \override Stem.stemlet-length = #0.5
  \tuplet 3/2 {r8[ c, g']}
}


