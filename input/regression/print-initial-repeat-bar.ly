\version "2.25.0"

\header {
  texidoc = "Using the @code{printInitialRepeatBar} property, repeat
bar lines can be printed automatically at the start of the piece."
}

{
  \set Score.printInitialRepeatBar = ##t
  \repeat volta 2 { c'1 }
}

{
  % printInitialRepeatBar respects startRepeatBarType
  \set Score.printInitialRepeatBar = ##t
  \set Score.startRepeatBarType = "[|:"
  \set Score.endRepeatBarType = ":|]"
  \repeat volta 2 { c'1 }
}

{
  % printInitialRepeatBar has no effect if no repeat starts at the beginning of
  % the piece.
  \set Score.printInitialRepeatBar = ##t
  % Use a bar type that would be visible at beginning-of-line if it were
  % printed, unlike \bar "|"
  \set Score.measureBarType = "|-s"
  c'1
  \repeat volta 2 { c'1 }
}
