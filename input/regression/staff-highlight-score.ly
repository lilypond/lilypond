\version "2.23.12"

\header {
  texidoc = "Highlights can be used in @code{Score}."
}

\layout {
  \context {
    \Staff
    \remove Staff_highlight_engraver
  }
  \context {
    \Score
    \consists Staff_highlight_engraver
  }
}

<<
  \new Staff { \staffHighlight lightsteelblue c'1 }
  \new Staff { c'1 }
>>
