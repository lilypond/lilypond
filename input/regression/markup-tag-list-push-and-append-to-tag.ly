\version "2.25.27"

\header {
  texidoc = "Test @code{\\push-to-tag} and @code{\\append-to-tag} on tagged lists of markups."
}

\markuplist {
  \override #'(padding . 2)
  \table
  #'(-1 -1)
  {
    \bold { Expectation Result }

    % first line of each snippet is the expectation
    % and the second line produces the result with tags to be tested

    "p A p B C"
    \push-to-tag #'foo p \line { \tag #'foo { A B } C }

    "p A B C"
    \push-to-tag #'foo p \line { \tag-list #'foo { A B } C }

    "p A B a C"
    \push-to-tag #'foo p
    \append-to-tag #'foo a
    \line { \tag-list #'foo { A B } C }

    "p A"
    \push-to-tag #'foo p
    \first-visible { \tag #'foo { A B } C }

    "p"
    \push-to-tag #'foo p
    \first-visible { \tag-list #'foo { A B } C }
  }
}
