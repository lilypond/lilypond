\version "2.19.16"

\header {
  texidoc = "The @code{\\compound-meter} markup command can produce various kinds of numeric time signature."
}

\markup {
  \vspace #2
  These are conventional time signatures:
  \compound-meter #3
  \compound-meter #'(3 . 4)
  \compound-meter #'(4 4)
  (Aren't they pretty?)
}

\markup {
  \vspace #2
  This is single-digit compound time signature:
  \compound-meter #'((2) (3))
  (Isn't it pretty?)
}

\markup {
  \vspace #2
  This is an unusual time signature:
  \compound-meter #'((6.22e23 1) (-4 . 3) (3.14) (9876 5432 0) (-1))
  (Isn't it pretty?)
}
