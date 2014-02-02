\version "2.19.2"
#(ly:set-option 'warning-as-error #f)
#(ly:expect-warning (_ "unterminated tie"))

\header {
  texidoc = "When a tie is followed only by unmatching notes and the tie cannot
  be created, lilypond prints out a warning unless @code{tieWaitForNote} is set."
}

\relative c' {
  c1~ 1 |
  c1~ <e c> |
  c1~ d |
  <c e>1~ c |
}
