\version "2.25.34"

\header{
  texidoc="If the body of a volta repeat is empty, the alternatives are still
rendered with the expected volta notation.  The expected volta numbers appear
below each staff."
}

#(ly:set-option 'warning-as-error #t)

testBody = { }
\include "repeat-volta-body-empty.ily"
