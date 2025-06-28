\version "2.25.27"

\header {
  texidoc = "This is a set of tests of the @code{beat-base} Scheme function for
single-fraction time signatures.  Problems are reported in the standard error
stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

#(expect-equal "signature not found in settings"
  (beat-base '(6 . 7) '())
  1/7)
#(expect-equal "signature not found in settings; senza misura"
  (beat-base #f '())
  +inf.0)
#(expect-equal "signature not found in settings; zero denominator"
  (beat-base '(1 . 0) '())
  +inf.0)
#(expect-equal "signature not found in settings; subdivided fraction"
  (beat-base '((5 12) . 13) '())
  1/13)

#(expect-equal "base not found in settings"
  (beat-base '(6 . 7) '(((6 . 7) . ())))
  1/7)
#(expect-equal "base not found in settings; senza misura"
  (beat-base #f '((#f . ())))
  +inf.0)
#(expect-equal "base not found in settings; zero denominator"
  (beat-base '(1 . 0) '(((1 . 0) . ())))
  +inf.0)
#(expect-equal "base not found in settings; subdivided fraction"
  (beat-base '((5 12) . 13) '((((5 12) . 13) . ())))
  1/13)

#(expect-equal "base found in settings"
  (beat-base '(6 . 8) '(((6 . 8) . ((beatBase . 1/3)))))
  1/3)
#(expect-equal "base found in settings; senza misura"
  (beat-base #f '((#f . ((beatBase . 17)))))
  17)
#(expect-equal "base found in settings; zero denominator"
  (beat-base '(1 . 0) '(((1 . 0) . ((beatBase . 1/12)))))
  1/12)
#(expect-equal "base found in settings; subdivided fraction"
  (beat-base '((5 12) . 13) '((((5 12) . 13) . ((beatBase . 1/17)))))
  1/17)
