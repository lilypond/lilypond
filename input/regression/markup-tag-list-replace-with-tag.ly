\version "2.25.32"

\header {
  texidoc = "Test @code{replace-with-tag} on tagged lists of markups."
}

\markuplist {
  \override #'(padding . 2)
  \table
  #'(-1 -1)
  {
    \bold { Expectation Result }

    % first line of each snippet is the expectation
    % and the second line produces the result with tags to be tested

    "p C"
    \replace-with-tag #'foo p \line { \tag-list #'foo { A B } C }

    "p p C"
    \replace-with-tag #'foo p
    \line { \tag #'foo { A B } C }

    "p C"
    \replace-with-tag #'foo p
    \line { \tag-list #'foo { A B } C }
  }
}
