\version "2.19.21"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "unterminated tie"))

\header {
  texidoc = "When a tie is followed only by unmatching notes and the tie cannot
  be created, lilypond prints out a warning unless @code{tieWaitForNote} is set."
}

\relative {
  c'1~ 1 |
  c1~ <e c> |
  c1~ d |
  <c e>1~ c |
}
