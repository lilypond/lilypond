\version "2.25.24"

\header {
  texidoc = "@code{\\popContextProperty} avoids reading the property stacks of
contexts enclosing the target context.  No instrument name should appear."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "cannot pop from empty stack"))

\new Staff {
  %% Create a stack in Score containing ("FAIL").
  \set Score.instrumentName = "FAIL"
  \pushContextProperty Score.instrumentName
  \set Score.instrumentName = "SCORE"
  \contextPropertyCheck Score.instrumentName "SCORE"
  \contextPropertyCheck Staff.instrumentName #'()

  %% This pop ignores the stack in Score and warns instead.  If it improperly
  %% read the stack in Score, it would set Staff.instrumentName to FAIL.
  \popContextProperty Staff.instrumentName
  \contextPropertyCheck Score.instrumentName "SCORE"
  \contextPropertyCheck Staff.instrumentName \default

  %% The stack in Score was not touched, so this pop does not warn.
  \popContextProperty Score.instrumentName
  \unset Score.instrumentName

  s
}
