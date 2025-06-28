\version "2.25.27"

\header {
  texidoc = "This is a set of tests of the @code{beat-base} Scheme function for
strictly alternating time signatures.  Problems are reported in the standard
error stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

#(expect-equal "no signatures found in settings"
  (beat-base
   '((1 . 11) (4 . 17) ((2 3) . 7))
   '())
  1/17)
#(expect-equal "no signatures found in settings; one denominator zero"
  (beat-base
   '((1 . 11) (4 . 0) ((2 3) . 7))
   '())
  1/11)
#(expect-equal "no signatures found in settings; all denominators zero"
  (beat-base
   '((1 . 0) (2 . 0))
   '())
  +inf.0)

%% We don't expect a strictly alternating time signature to have its own entry
%% in the settings, but there is value in testing it.
#(expect-equal "full signature found in settings; no base found"
  (beat-base
   '((1 . 11) (4 . 17))
   '((((1 . 11) (4 . 17)) . ())))
  1/17)
#(expect-equal "full signature found in settings; base found"
  (beat-base
   '((1 . 11) (4 . 17))
   '((((1 . 11) (4 . 17)) . ((beatBase . 1/13)))))
  1/13)

#(expect-equal "first component base found in settings"
  (beat-base
   '((1 . 2) (3 . 4))
   '(((1 . 2) . ((beatBase . 1/5)))))
  1/5)
#(expect-equal "second component base found in settings"
  (beat-base
   '((1 . 2) (3 . 4))
   '(((3 . 4) . ((beatBase . 1/7)))))
  1/7)
#(expect-equal "all component bases found in settings; all finite"
  (beat-base
   '((1 . 2) (3 . 4) (5 . 6))
   '(((1 . 2) . ((beatBase . 1/11)))
     ((3 . 4) . ((beatBase . 1/5)))
     ((5 . 6) . ((beatBase . 1/7)))))
  1/11)
#(expect-equal "all component bases found in settings; one +inf.0"
  (beat-base
   '((1 . 2) (3 . 4))
   '(((1 . 2) . ((beatBase . 1/9)))
     ((3 . 4) . ((beatBase . +inf.0)))))
  1/9)
#(expect-equal "all component bases found in settings; all +inf.0"
  (beat-base
   '((1 . 2) (3 . 4))
   '(((1 . 2) . ((beatBase . +inf.0)))
     ((3 . 4) . ((beatBase . +inf.0)))))
  +inf.0)
