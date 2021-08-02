\header {
    texidoc = "Grace notes in parts are combined."
}

\version "2.21.0"
\layout { ragged-right = ##t }

\score {
  \new Voice = "v" {
    r2
    \partCombine { \grace { a'8 g'8 } e'4 \grace g'8 e'4 }
                 { \grace e'8 c'4 \grace e'8 c'4 }
  }
}
