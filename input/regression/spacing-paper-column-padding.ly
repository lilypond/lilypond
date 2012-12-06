\version "2.17.6"

\header {
  texidoc = "The space after a paper column can be increased by overriding
the padding property."
}

\layout {
  \context {
    \Score
    \override PaperColumn.padding = #10
    \override NonMusicalPaperColumn.padding = #10
  }
}

{a b}
