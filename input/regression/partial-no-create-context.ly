\version "2.25.28"

\header {
  texidoc = "@code{\\partial} does not create a @code{Bottom} context.  The
output should be a single staff that ends at the end of measure@tie{}1."
}

#(ly:set-option 'warning-as-error #t)

\new Score \with {
  barNumberVisibility = #(every-nth-bar-number-visible 1)
  \override BarNumber.break-visibility = #all-visible
} <<
  \partial 4
  \new Staff { f'4 4 4 4 4 }
>>
