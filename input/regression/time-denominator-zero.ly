\version "2.25.25"

\header {
  texidoc = "@code{\\time} with a zero denominator results in a warning."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "unsupported time signature"))

{ \time 1/0 c'1 }
