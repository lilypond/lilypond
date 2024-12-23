\version "2.25.24"

\header {
  texidoc = "@code{\\popContextProperty} warns when the stack is empty and
unsets the property.  No instrument name should appear."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "cannot pop from empty stack; unsetting"))

\new Staff \with { instrumentName = "FAIL" } {
  \popContextProperty Staff.instrumentName
  \contextPropertyCheck Staff.instrumentName \default
  s
}
