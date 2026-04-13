\version "2.25.35"

\header {
  texidoc = "Pages can be numbered per bookpart rather than per book."
}

#(set-default-paper-size "a8")

\paper {
  bookpart-level-page-numbering = ##t
}

\book {
  \bookpart {
    \*5 { d'1 \pageBreak }
  }
  \bookpart {
    \*5 { c'1 \pageBreak }
  }
}
