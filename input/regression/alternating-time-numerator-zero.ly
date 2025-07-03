\version "2.25.27"

\header {
  texidoc = "A strictly alternating time signature with a zero in the numerator
results in a warning."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "unsupported time signature"))

{ \compoundMeter #'((1 0 2) (3 4)) c'1 }
