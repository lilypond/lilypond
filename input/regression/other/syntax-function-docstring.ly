\version "2.23.8"

\header {
  texidoc = "Syntax functions' docstrings are preserved (mainly for the sake
of doc autogeneration)."
}

funOne =
#(define-music-function (m) (ly:music?)
   "Docstring"
   m)

funTwo =
#(define-music-function (m) (ly:music?)
   ;; no docstring
   m)

funThree =
#(define-music-function (m) (ly:music?)
   ;; docstring marked translatable should be kept
   (_i "Docstring")
   m)

funFour =
#(define-scheme-function (m) (ly:music?)
   ;; This is not a docstring but the expression that the function returns.
   (_i "String"))

#(for-each
  (lambda (fun)
    (ly:message "Found docstring: ~s" (procedure-documentation (ly:music-function-extract fun))))
  (list funOne funTwo funThree funFour))

#(when (not (equal? "String" (funFour #{ #})))
   (ly:error "failed test syntax-function-docstring.ly"))
