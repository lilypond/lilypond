\version "2.27.2"

\header {
  texidoc = "@code{\\setMeasureLengthFromHere} does not create a @code{Bottom}
context.  The output should be a single staff that ends at the end of
measure@tie{}2.  Measure@tie{}1 is irregular, having only one beat."
}

#(ly:set-option 'warning-as-error #t)

\new Score \with {
  barNumberVisibility = #(every-nth-bar-number-visible 1)
  \override BarNumber.break-visibility = #all-visible
} <<
  {
    \setMeasureLengthFromHere 4
    \skip 4
    \setDefaultMeasureLength
  }
  \new Staff { f'4 | 4 4 4 4 | }
>>
