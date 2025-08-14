\version "2.25.28"

\header {
  texidoc = "This is a set of tests of the @code{time-signature->@/fraction}
Scheme function.  Problems are reported in the standard error stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

#(expect-equal "senza misura"
  (time-signature->fraction #f)
  #f)
#(expect-equal "simple fraction"
  (time-signature->fraction '(6 . 7))
  '(6 . 7))
#(expect-equal "subdivided fraction"
  (time-signature->fraction '((5 12) . 13))
  '(17 . 13))
#(expect-equal "strictly alternating"
  (time-signature->fraction '((3 . 4) (6 . 8)))
  '(12 . 8))
