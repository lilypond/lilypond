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
#(expect-equal "signature not found in settings; custom base"
  (beat-structure 1/8 '(4 . 4) '())
  '(2 2 2 2))
#(expect-equal "signature not found in settings; senza-misura base"
  (beat-structure +inf.0 '(4 . 4) '())
  '())
%% robustness tests
#(expect-equal "signature not found in settings; zero numerator"
  (beat-structure 1/4 '(0 . 3) '())
  '())
#(expect-equal "signature not found in settings; zero denominator"
  (beat-structure 1/4 '(2 . 0) '())
  '(+inf.0))
#(expect-equal "signature not found in settings; zero base"
  (beat-structure 0 '(4 . 4) '())
  '(+inf.0))

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
#(expect-equal "structure not found in settings; custom base"
  (beat-structure 1/8 '(4 . 4) testSettings)
  '(2 2 2 2))
#(expect-equal "structure not found in settings; senza-misura base"
  (beat-structure +inf.0 '(4 . 4) testSettings)
  '())
%% robustness tests
#(expect-equal "structure not found in settings; zero numerator"
  (beat-structure 1/4 '(0 . 3) testSettings)
  '())
#(expect-equal "structure not found in settings; zero denominator"
  (beat-structure 1/4 '(2 . 0) testSettings)
  '(+inf.0))
#(expect-equal "structure not found in settings; zero base"
  (beat-structure 0 '(4 . 4) testSettings)
  '(+inf.0))

#(expect-equal "structure and base found in settings; same base"
  (beat-structure 1/4 '(6 . 8) testSettings)
  '(1 1 1))
#(expect-equal "structure and base found in settings; different base"
  (beat-structure 1/8 '(6 . 8) testSettings)
  '(2 2 2))
#(expect-equal "structure and base found in settings; senza-misura base"
  (beat-structure +inf.0 '(6 . 8) testSettings)
  '())
%% robustness test
#(expect-equal "structure and base found in settings; zero base"
  (beat-structure 0 '(6 . 8) testSettings)
  '(+inf.0))

#(expect-equal "structure found in settings; default base"
  (beat-structure 1/8 '(8 . 8) testSettings)
  '(2 3 3))
#(expect-equal "structure found in settings; custom base"
  (beat-structure 1/16 '(8 . 8) testSettings)
  '(4 6 6))
#(expect-equal "structure found in settings; senza-misura base"
  (beat-structure +inf.0 '(8 . 8) testSettings)
  '())
%% robustness test
#(expect-equal "structure found in settings; zero base"
  (beat-structure 0 '(8 . 8) testSettings)
  '(+inf.0))
