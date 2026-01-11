\version "2.25.32"

\header {
  texidoc = "This is a set of tests of the @code{calc-submeasure-structure}
Scheme function for single-fraction time signatures.  Problems are reported in
the standard error stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

#(expect-equal "signature not found in settings; default base"
  (calc-submeasure-structure 1/4 '(4 . 4) '())
  '(4))
#(expect-equal "signature not found in settings; custom base"
  (calc-submeasure-structure 1/8 '(4 . 4) '())
  '(8))
#(expect-equal "signature not found in settings; senza-misura base"
  (calc-submeasure-structure +inf.0 '(4 . 4) '())
  '())
#(expect-equal "signature not found in settings; subdiv. w/ default base"
  (calc-submeasure-structure 1/8 '((2 3) . 8) '())
  '(5))
#(expect-equal "signature not found in settings; subdiv. w/ custom base"
  (calc-submeasure-structure 1/16 '((2 3) . 8) '())
  '(10))
#(expect-equal "signature not found in settings; subdiv. w/ senza-misura base"
  (calc-submeasure-structure +inf.0 '((2 3) . 8) '())
  '())
%% robustness tests
#(expect-equal "signature not found in settings; zero numerator"
  (calc-submeasure-structure 1/4 '(0 . 3) '())
  '())
#(expect-equal "signature not found in settings; zero denominator"
  (calc-submeasure-structure 1/4 '(2 . 0) '())
  '(+inf.0))
#(expect-equal "signature not found in settings; zero base"
  (calc-submeasure-structure 0 '(4 . 4) '())
  '(+inf.0))
#(expect-equal "subdivided signature not found in settings; zero numerator"
  (calc-submeasure-structure 1/4 '((0 0) . 3) '())
  '())
#(expect-equal "subdivided signature not found in settings; zero denominator"
  (calc-submeasure-structure 1/4 '((1 2) . 0) '())
  '(+inf.0))
#(expect-equal "subdivided signature not found in settings; zero base"
  (calc-submeasure-structure 0 '((3 4) . 4) '())
  '(+inf.0))

testSettings = #'(
  ((2 . 0) . ())
  ((0 . 3) . ())
  ((4 . 4) . ())
  (((4 3) . 2) . ())
  ((6 . 8) . ((beatBase . 1/4)
              (submeasureStructure . (1 1 1))))
  (((3 2 2) . 8) . ((beatBase . 1/16)
                    (submeasureStructure . (6 8))))
  (((2 2 3) . 8) . ((submeasureStructure . (4 3))))
  ((8 . 8) . ((submeasureStructure . (2 3 3))))
)

#(expect-equal "structure not found in settings; default base"
  (calc-submeasure-structure 1/4 '(4 . 4) testSettings)
  '(4))
#(expect-equal "structure not found in settings; custom base"
  (calc-submeasure-structure 1/8 '(4 . 4) testSettings)
  '(8))
#(expect-equal "structure not found in settings; senza-misura base"
  (calc-submeasure-structure +inf.0 '(4 . 4) testSettings)
  '())
#(expect-equal "structure not found in settings; subdiv. w/ default base"
  (calc-submeasure-structure 1/2 '((4 3) . 2) testSettings)
  '(7))
#(expect-equal "structure not found in settings; subdiv. w/ custom base"
  (calc-submeasure-structure 1/4 '((4 3) . 2) testSettings)
  '(14))
#(expect-equal "structure not found in settings; subdiv. w/ senza-misura base"
  (calc-submeasure-structure +inf.0 '((4 3) . 2) testSettings)
  '())
%% robustness tests
#(expect-equal "structure not found in settings; zero numerator"
  (calc-submeasure-structure 1/4 '(0 . 3) testSettings)
  '())
#(expect-equal "structure not found in settings; zero denominator"
  (calc-submeasure-structure 1/4 '(2 . 0) testSettings)
  '(+inf.0))
#(expect-equal "structure not found in settings; zero base"
  (calc-submeasure-structure 0 '(4 . 4) testSettings)
  '(+inf.0))
#(expect-equal "subdivided structure not found in settings; zero numerator"
  (calc-submeasure-structure 1/4
   '((0 0) . 3)
   '((((0 0) . 3) . ())))
  '())
#(expect-equal "subdivided structure not found in settings; zero denominator"
  (calc-submeasure-structure 1/4
   '((1 2) . 0)
   '((((1 2) . 0) . ())))
  '(+inf.0))
#(expect-equal "subdivided structure not found in settings; zero base"
  (calc-submeasure-structure 0
   '((3 4) . 4)
   '((((3 4) . 4) . ())))
  '(+inf.0))

#(expect-equal "structure and base found in settings; same base"
  (calc-submeasure-structure 1/4 '(6 . 8) testSettings)
  '(1 1 1))
#(expect-equal "structure and base found in settings; different base"
  (calc-submeasure-structure 1/8 '(6 . 8) testSettings)
  '(2 2 2))
#(expect-equal "structure and base found in settings; senza-misura base"
  (calc-submeasure-structure +inf.0 '(6 . 8) testSettings)
  '())
#(expect-equal "structure and base found; subdiv. w/ default base"
  (calc-submeasure-structure 1/16 '((3 2 2) . 8) testSettings)
  '(6 8))
#(expect-equal "structure and base found; subdiv. w/ custom base"
  (calc-submeasure-structure 1/8 '((3 2 2) . 8) testSettings)
  '(3 4))
#(expect-equal "structure and base found; subdiv. w/ senza-misura base"
  (calc-submeasure-structure +inf.0 '((3 2 2) . 8) testSettings)
  '())
%% robustness test
#(expect-equal "structure and base found in settings; zero base"
  (calc-submeasure-structure 0 '(6 . 8) testSettings)
  '(+inf.0))

#(expect-equal "structure found in settings; default base"
  (calc-submeasure-structure 1/8 '(8 . 8) testSettings)
  '(2 3 3))
#(expect-equal "structure found in settings; custom base"
  (calc-submeasure-structure 1/16 '(8 . 8) testSettings)
  '(4 6 6))
#(expect-equal "structure found in settings; senza-misura base"
  (calc-submeasure-structure +inf.0 '(8 . 8) testSettings)
  '())
#(expect-equal "structure found in settings; subdiv. w/ default base"
  (calc-submeasure-structure 1/8 '((2 2 3) . 8) testSettings)
  '(4 3))
#(expect-equal "structure found in settings; subdiv. w/ custom base"
  (calc-submeasure-structure 1 '((2 2 3) . 8) testSettings)
  '(4/8 3/8))
#(expect-equal "structure found in settings; subdiv. w/ senza-misura base"
  (calc-submeasure-structure +inf.0 '((2 2 3) . 8) testSettings)
  '())
%% robustness test
#(expect-equal "structure found in settings; zero base"
  (calc-submeasure-structure 0 '(8 . 8) testSettings)
  '(+inf.0))
