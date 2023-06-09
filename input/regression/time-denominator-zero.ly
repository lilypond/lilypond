\version "2.25.6"

\header {
  texidoc = "@code{\\time} with a zero denominator results in a warning."
}

#(ly:set-option 'warning-as-error #t)

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' must be of type '%s', ignoring invalid value '%s'")
   "timeSignatureFraction" "positive, finite fraction, as pair" '(1 . 0))

{ \time 1/0 c'1 }
