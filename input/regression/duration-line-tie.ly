\version "2.23.4"

\header {
  texidoc = "@code{DurationLine} should pass a tied @code{NoteHead}, but a
previous @code{Tie} should not be taken into account.
In this test the @code{DurationLine}, starting at the third note of each line,
should end before the fourth note of said line."
}

\score {
  {
    a'1~\- a'1 g'1\- f'
    \break
    a'1~ a' g'\- f'
  }
  \layout {
    \context {
      \Voice
      \consists Duration_line_engraver
    }
  }
}
