\version "2.23.7"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "This test shows the placement of dashed bar lines with
span bars in various staff configurations."
}

testBar = "!"
\include "bar-line-placement-span.ily"
