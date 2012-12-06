\version "2.17.6"

\header {
  texidoc = "Rehearsal marks with direction DOWN get placed at
the bottom of the score."
}

\new StaffGroup <<
   \new Staff { g'1 }
   \new Staff {
     c'1
     \once \override Score.RehearsalMark.break-visibility = #begin-of-line-invisible
     \once \override Score.RehearsalMark.direction = #DOWN
     \mark \markup \italic "Fine."
   }
 >>
