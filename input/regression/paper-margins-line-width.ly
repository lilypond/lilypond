\version "2.19.21"

\header {
  texidoc = "If only line-width is given, systems are horizontally centered."
}

someNotes = \relative { \repeat unfold 40 { c'4 d e f } }

\paper {
  line-width = 100 \mm
}

\book {
  \score { \someNotes }
}
