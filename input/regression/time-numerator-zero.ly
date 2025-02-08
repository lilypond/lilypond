\version "2.25.25"

\header {
  texidoc = "@code{\\time} with a zero numerator results in a warning."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "unsupported time signature"))

{ \time 0/1 c'1 }
