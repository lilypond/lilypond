\version "2.27.5"

\header {
  texidoc = "This is a set of tests of the @code{structured/} Scheme function.
Problems are reported in the standard error stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

#(use-modules (srfi srfi-11)) % let-values

%%----------------------------------------------------------------------

#(expect-equal "propagate NaN through dividend"
  (let-values (((q r) (structured/ +nan.0 '(1 2 3))))
   (list q r))
  '(+nan.0 (+nan.0 +nan.0)))

#(expect-equal "0/{∞}"
  (let-values (((q r) (structured/ 0 '(+inf.0))))
   (list q r))
  '(0 (0 0)))

#(expect-equal "nonzero/{∞}"
  (let-values (((q r) (structured/ 2 '(+inf.0))))
   (list q r))
  '(0 (0 2)))

#(expect-equal "nonzero/{..., ∞}"
  (let-values (((q r) (structured/ 7 '(5 5 +inf.0))))
   (list q r))
  '(0 (1 2)))

#(expect-equal "∞/{∞}"
  (let-values (((q r) (structured/ +inf.0 '(+inf.0))))
   (list q r))
  '(+nan.0 (+nan.0 +nan.0)))

#(expect-equal "∞/{..., ∞}"
  (let-values (((q r) (structured/ +inf.0 '(5 +inf.0))))
   (list q r))
  '(+nan.0 (+nan.0 +nan.0)))

%%----------------------------------------------------------------------

#(expect-equal "negative dividend"
  (let-values (((q r) (structured/ -15/10 '(3/10 4/10 5/10))))
   (list q r))
  '(-2 (2 2/10)))

%%----------------------------------------------------------------------

#(expect-equal "zero dividend"
  (let-values (((q r) (structured/ 0 '(1/4 1/4))))
   (list q r))
  '(0 (0 0)))

#(expect-equal "one beat"
  (let-values (((q r) (structured/ 1/123 '(1/4))))
   (list q r))
  '(0 (0 1/123)))

#(expect-equal "two beats"
  (let-values (((q r) (structured/ 1/4 '(1/4 1/4))))
   (list q r))
  '(0 (1 0)))

#(expect-equal "two beats with remainder"
  (let-values (((q r) (structured/ 3/8 '(1/4 1/4))))
   (list q r))
  '(0 (1 1/8)))

#(expect-equal "full measure"
  (let-values (((q r) (structured/ 4/4 '(1/4 1/4 1/4 1/4))))
   (list q r))
  '(1 (0 0)))

#(expect-equal "multiple measures"
  (let-values (((q r) (structured/ 904/9 '(1/3 1/3 1/3))))
   (list q r))
  '(100 (1 1/9)))

%%----------------------------------------------------------------------

#(expect-equal "structured/ quotient is directly usable as first value"
  (structured/ -15/10 '(3/10 4/10 5/10))
  -2)

#(expect-equal "structured-remainder"
  (structured-remainder -15/10 '(3/10 4/10 5/10))
  '(2 2/10))
