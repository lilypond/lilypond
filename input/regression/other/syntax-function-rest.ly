\version "2.23.8"

\header {
  texidoc = "Syntax functions can use rest arguments."
}

fun =
#(define-scheme-function ((x . y) . z) (integer? integer? integer?)
   (list x y z))

#(when (not (equal? '(1 (2 3) ()) ((fun 1 2 3))))
       (ly:error "failed test syntax-function-rest.ly"))
