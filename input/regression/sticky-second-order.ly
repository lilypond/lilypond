\version "2.23.4"

\header {
  texidoc = "Sticky grobs can be attached to other sticky grobs."
}

#(set-default-paper-size "a7landscape")

\book {
  \new Score \with {
    \consists Balloon_engraver
  }
  {
    \balloonGrobText Footnote #'(1 . 1.5) "a footnote"
    \once \override Score.Footnote.X-extent = #'(1 . 3)
    \once \override Score.Footnote.Y-extent = #'(-0.3 . 2.5)
    \footnote #'(2 . 2) "note" NoteHead
    c'
  }
}
