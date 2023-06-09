\version "2.25.6"

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
   "measureLength" "positive moment with no grace part" ZERO-MOMENT)

{ \time 0/1 c'1 }
