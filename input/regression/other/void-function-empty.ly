\version "2.23.8"

\header {
  texidoc = "A void function's body can be empty."
}

noop =
#(define-void-function () ())

\noop
