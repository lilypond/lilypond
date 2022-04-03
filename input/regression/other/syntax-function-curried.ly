\version "2.23.8"

\header {
  texidoc = "Syntax functions can use curried definitions."
}

curried =
#(define-scheme-function (((a b) c) d) ((integer? 5) string?)
   "Docstring"
   a)

partial = \curried "a"

#(when (not (eqv? 5 ((partial 'x) 'y)))
       (ly:error "failed test syntax-function-curried.ly"))

#(ly:message "curried has the docstring: ~s"
             (procedure-documentation (ly:music-function-extract curried)))
