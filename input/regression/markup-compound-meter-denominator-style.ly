\version "2.25.25"

\header {
  texidoc = "The @code{\\compound-meter} markup command can produce various
kinds of numeric time signature.  The left column tells the value of the
@code{denominator-style} property."
}

theRow = \markuplist {
  \on-the-fly #(lambda (layout props m)
                (interpret-markup layout props
                 (make-normal-text-markup
                  (symbol->string
                   (chain-assoc-get 'denominator-style props
                    (string->symbol "(default)"))))))
  ""
  \compound-meter #3
  \compound-meter #'((2) (3))
  \compound-meter #'(4 4)
  \compound-meter #'((2 8/3) (3 4))
  \compound-meter #'(2 3 8)
  \compound-meter #'((2 3 8) (1 1 4))
  \compound-meter #'((3.14 2) (4 . -3) (+inf.0) (9 5 0) (-1))
}

\markuplist {
  \override #'(baseline-skip . 5)
  \override #'(padding . 3)
  \table #'(-1 -1 -1 -1 -1 -1 -1 -1) {
    \theRow
    \override #'(denominator-style . none)
    \theRow
    \override #'(denominator-style . note)
    \theRow
    \override #'(denominator-style . number)
    \theRow
  }
}
