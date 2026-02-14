\version "2.25.34"

\header{
  texidoc="If a volta repeat alternative is a non-empty music expression
with a duration of zero, the other alternatives are still rendered with the
expected volta notation.  The expected volta numbers appear below each staff."
}

#(ly:set-option 'warning-as-error #t)

testMusic = s1*0
\include "repeat-volta-alternative-empty.ily"
