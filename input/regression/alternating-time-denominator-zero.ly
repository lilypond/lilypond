\version "2.25.33"

\header {
  texidoc = "A strictly alternating time signature with a zero denominator
results in a warning."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "unsupported time signature"))

{ \timeAbbrev #'((1 2) (3 0)) c'1 }
