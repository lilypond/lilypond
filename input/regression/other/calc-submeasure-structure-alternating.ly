\version "2.25.32"

\header {
  texidoc = "This is a set of tests of the @code{calc-submeasure-structure}
Scheme function for strictly alternating signatures.  Problems are reported in
the standard error stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

#(expect-equal "no signatures found in settings; default base"
  (calc-submeasure-structure 1/8
   '((4 . 4) (6 . 8))
   '())
  '(8 6))
#(expect-equal "no signatures found in settings; custom base"
  (calc-submeasure-structure 3/8
   '((4 . 4) (6 . 8))
   '())
  '(8/3 2))
#(expect-equal "no signatures found in settings; senza-misura base"
  (calc-submeasure-structure +inf.0
   '((4 . 4) (6 . 8))
   '())
  '())
#(expect-equal "no signatures found in settings; one denominator zero"
  (calc-submeasure-structure 1/4
   '((4 . 4) (1 . 0) (3 . 4))
   '())
  '(4 +inf.0 3)) % just '(+inf.0) would be fine too
#(expect-equal "no signatures found in settings; all denominators zero"
  (calc-submeasure-structure 1
   '((1 . 0) (2 . 0))
   '())
  '(+inf.0 +inf.0)) % just '(+inf.0) would be fine too

%% We don't expect a strictly alternating time signature to have its own entry
%% in the settings, but there is value in testing it.
#(expect-equal "full signature found in settings; no structure found"
  (calc-submeasure-structure 1/8
   '((4 . 4) (6 . 8))
   '())
  '(8 6))
#(expect-equal "full signature found in settings; senza-misura base"
  (calc-submeasure-structure +inf.0
   '((4 . 4) (6 . 8))
   '())
  '())
#(expect-equal "full signature found in settings; structure found"
  (calc-submeasure-structure 1/4
   '((4 . 4) (6 . 8))
   '((((4 . 4) (6 . 8)) . (; default beatBase is 1/8
                           (submeasureStructure . (5 4 3 2))))))
  '(5/2 4/2 3/2 2/2))
#(expect-equal "full signature found in settings; structure and base found"
  (calc-submeasure-structure 1/8
   '((4 . 4) (6 . 8))
   '((((4 . 4) (6 . 8)) . ((beatBase . 1/4)
                           (submeasureStructure . (1 2 4))))))
  '(2 4 8))

#(expect-equal "first component found in settings"
  (calc-submeasure-structure 1/4
   '((1 . 2) (3 . 4))
   '(((1 . 2) . ((beatBase . 1/8)
                 (submeasureStructure . (2 2))))))
  '(1 1  3))
#(expect-equal "second component found in settings"
  (calc-submeasure-structure 1/8
   '((1 . 2) (3 . 4))
   '(((3 . 4) . (; default beatBase is 1/4
                 (submeasureStructure . (1 2))))))
  '(4  2 4))
#(expect-equal "all components found in settings"
  (calc-submeasure-structure 1
   '((1 . 2) (3 . 4) (5 . 6))
   '(((1 . 2) . (; default beatBase is 1/2
                 (submeasureStructure . (1/2 1/2))))
     ((3 . 4) . (; default beatBase is 1/4
                 (submeasureStructure . (3/2 3/2))))
     ((5 . 6) . (; default beatBase is 1/6
                 (submeasureStructure . (5/2 5/2))))))
  '(1/4 1/4 3/8 3/8 5/12 5/12))
