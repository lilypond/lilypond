\version "2.25.27"

\header {
  texidoc = "This is a set of tests of the @code{beat-structure} Scheme function
for single-fraction time signatures.  Problems are reported in the standard
error stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

#(expect-equal "signature not found in settings; default base"
  (beat-structure 1/4 '(4 . 4) '())
  '(1 1 1 1))
%% robustness tests
#(expect-equal "signature not found in settings; zero numerator"
  (beat-structure 1/4 '(0 . 3) '())
  '())

testSettings = #'(
  ((2 . 0) . ())
  ((0 . 3) . ())
  ((4 . 4) . ())
  ((6 . 8) . ((beatBase . 1/4)
              (beatStructure . (1 1 1))))
  ((8 . 8) . ((beatStructure . (2 3 3))))
)

#(expect-equal "structure not found in settings; default base"
  (beat-structure 1/4 '(4 . 4) testSettings)
  '(1 1 1 1))
%% robustness tests
#(expect-equal "structure not found in settings; zero numerator"
  (beat-structure 1/4 '(0 . 3) testSettings)
  '())

#(expect-equal "structure and base found in settings; same base"
  (beat-structure 1/4 '(6 . 8) testSettings)
  '(1 1 1))

#(expect-equal "structure found in settings; default base"
  (beat-structure 1/8 '(8 . 8) testSettings)
  '(2 3 3))
