\header {
    texidoc = "Grace notes in parts are combined."
}

\version "2.19.22"
\layout { ragged-right = ##t }

\score {
  \new Voice = "v" {
    r2
    \partcombine { \grace g'8 e'4 \grace g'8 e'4 }
                 { \grace e'8 c'4 \grace e'8 c'4 }
  }
}
