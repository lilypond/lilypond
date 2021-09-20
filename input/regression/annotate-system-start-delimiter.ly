\version "2.23.4"

\header {
  texidoc = "Footnotes and balloons also work on system start delimiters."
}

#(set-default-paper-size "a7landscape")

\book {
  \new Score \with {
    \consists Balloon_engraver
    systemStartDelimiter = #'SystemStartBracket
  }
  <<
    \new Staff {
      \balloonGrobText SystemStartBracket #'(1 . -1) "text"
      \footnote #'(-1 . 0.2) "note" Score.SystemStartBracket
      c'
    }
    \new Staff { c' }
  >>
}
