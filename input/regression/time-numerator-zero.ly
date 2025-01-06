\version "2.25.23"

\header {
  texidoc = "@code{\\time} with a zero numerator results in a warning."
}

#(ly:set-option 'warning-as-error #t)

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' must be of type '%s', ignoring invalid value '%s'")
   "timeSignatureFraction" "positive, finite fraction, as pair" '(0 . 1))

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' must be of type '%s', ignoring invalid value '%s'")
   "measureLength" "positive exact rational or +inf.0" 0)

{ \time 0/1 c'1 }
