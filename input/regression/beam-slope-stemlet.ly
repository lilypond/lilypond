
\header {

  texidoc = "For slope calculations, stemlets are treated
as invisible stems."

}

\version "2.12.0"

\layout {
  ragged-right = ##t
  indent = #0
}

\relative c' {
  \times 2/3 {r8[ c' g']}
  \override Stem #'stemlet-length = #0.5
  \times 2/3 {r8[ c, g']}
}


