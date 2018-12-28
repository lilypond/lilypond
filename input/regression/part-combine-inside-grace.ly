\header {
    texidoc = "The notes of the first chord share a stem but the notes of the second chord do not."
}

\version "2.21.0"
\layout { ragged-right = ##t }

\score {
  \context Voice = "v" {
    r2
    \grace { \partCombine \relative { e'16 e }
                          \relative { c'16 f } }
    c'2
  }
}
