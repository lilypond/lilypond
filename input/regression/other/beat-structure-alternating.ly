\version "2.25.27"

\header {
  texidoc = "This is a set of tests of the @code{beat-structure} Scheme function
for strictly alternating signatures.  Problems are reported in the standard
error stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

#(expect-equal "no signatures found in settings; default base"
  (beat-structure 1/8
   '((4 . 4) (6 . 8))
   '())
  '(2 2 2 2  3 3))
#(expect-equal "no signatures found in settings; custom base"
  (beat-structure 3/8
   '((4 . 4) (6 . 8))
   '())
  '(2/3 2/3 2/3 2/3  1 1))
#(expect-equal "no signatures found in settings; senza-misura base"
  (beat-structure +inf.0
   '((4 . 4) (6 . 8))
   '())
  '())
#(expect-equal "no signatures found in settings; one denominator zero"
  (beat-structure 1/4
   '((4 . 4) (1 . 0) (3 . 4))
   '())
  '(1 1 1 1 +inf.0 1 1 1)) % just '(+inf.0) would be fine too
#(expect-equal "no signatures found in settings; all denominators zero"
  (beat-structure 1
   '((1 . 0) (2 . 0))
   '())
  '(+inf.0 +inf.0)) % just '(+inf.0) would be fine too

%% We don't expect a strictly alternating time signature to have its own entry
%% in the settings, but there is value in testing it.
#(expect-equal "full signature found in settings; no structure found"
  (beat-structure 1/8
   '((4 . 4) (6 . 8))
   '())
  '(2 2 2 2  3 3))
#(expect-equal "full signature found in settings; senza-misura base"
  (beat-structure +inf.0
   '((4 . 4) (6 . 8))
   '())
  '())
#(expect-equal "full signature found in settings; structure found"
  (beat-structure 1/4
   '((4 . 4) (6 . 8))
   '((((4 . 4) (6 . 8)) . (; default beatBase is 1/8
                           (beatStructure . (8 6))))))
  '(4 3))
#(expect-equal "full signature found in settings; structure and base found"
  (beat-structure 1/8
   '((4 . 4) (6 . 8))
   '((((4 . 4) (6 . 8)) . ((beatBase . 1/4)
                           (beatStructure . (4 3))))))
  '(8 6))

#(expect-equal "first component found in settings"
  (beat-structure 1/4
   '((1 . 2) (3 . 4))
   '(((1 . 2) . ((beatBase . 1/6)
                 (beatStructure . (3))))))
  '(2  1 1 1))
#(expect-equal "second component found in settings"
  (beat-structure 1/8
   '((1 . 2) (3 . 4))
   '(((3 . 4) . (; default beatBase is 1/4
                 (beatStructure . (1 2))))))
  '(4  2 4))
#(expect-equal "all components found in settings"
  (beat-structure 1
   '((1 . 2) (3 . 4) (5 . 6))
   '(((1 . 2) . (; default beatBase is 1/2
                 (beatStructure . (1))))
     ((3 . 4) . (; default beatBase is 1/4
                 (beatStructure . (3))))
     ((5 . 6) . (; default beatBase is 1/6
                 (beatStructure . (5))))))
  '(1/2 3/4 5/6))
