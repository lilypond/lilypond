\version "2.23.12"

\header {
  texidoc = "Pages can be numbered per bookpart rather than per book."
}

#(set-default-paper-size "a8")

\paper {
  bookpart-level-page-numbering = ##t
}

\book {
  \bookpart {
    \repeat unfold 5 { d'1 \pageBreak }
  }
  \bookpart {
    \repeat unfold 5 { c'1 \pageBreak }
  }
}
