\version "2.25.27"

\header {
  texidoc = "This is a set of tests of the @code{calc-measure-length} Scheme
function.  Problems are reported in the standard error stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

#(expect-equal "simple"
  (calc-measure-length '(6 . 7))
  6/7)

#(expect-equal "subdivided"
  (calc-measure-length '((2 3 2) . 11))
  7/11)

#(expect-equal "senza misura"
  (calc-measure-length #f)
  +inf.0)

#(expect-equal "zero numerator; simple"
  (calc-measure-length '(0 . 1))
  0)

#(expect-equal "zero numerator; subdivided"
  (calc-measure-length '((0 0) . 1))
  0)

#(expect-equal "zero denominator; simple"
  (calc-measure-length '(1 . 0))
  +inf.0)

#(expect-equal "zero denominator; subdivided"
  (calc-measure-length '((1 2) . 0))
  +inf.0)

#(expect-equal "strictly alternating"
  (calc-measure-length '((1 . 2) (0 . 9999) ((3 4) . 5)))
  19/10)
