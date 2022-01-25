\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "@code{startGraceMusic} and @code{stopGraceMusic} may be
overridden to change the properties of grace notes.  In this test, the
stems of the grace notes point down."
}

startGraceMusic = \stemDown
stopGraceMusic = \stemNeutral

{
  \grace a32
  f4
  \grace a32
  f4
}
