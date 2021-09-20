\version "2.23.4"

\header {
  texidoc = "Footnotes and balloons also work on volta brackets running
to the end of the piece."
}

#(set-default-paper-size "a7landscape")

\book {
  {
    \repeat volta 2 { c'1 }
    \alternative {
      { c'1 }
      {
        \footnote #'(1 . 0.05) "note" Score.VoltaBracket
        c'1
      }
    }
  }

  \new Score \with {
    \consists Balloon_engraver
  }
  {
    \repeat volta 2 { c'1 }
    \alternative {
      { c'1 }
      {
        \balloonGrobText VoltaBracket #'(2 . 2) "text"
        c'1
      }
    }
  }
}
