\version "2.25.27"

\header {
  texidoc = "Test simple @code{\\push-to-tag} and @code{\\append-to-tag} on tagged markups."
}

tagged = \markup { \tag #'foo A \tag #'bar B C }

\markuplist {
  \override #'(padding . 2)
  \table
  #'(-1 -1)
  {
    \bold { Expectation Result }

    % first line of each snippet is the expectation
    % and the second line produces the result with tags to be tested

    "p A B C"
    \push-to-tag #'foo p \tagged

    "A p B C"
    \push-to-tag #'bar p \tagged

    "A a B C"
    \append-to-tag #'foo a \tagged

    "p1 p2 A a2 a1 B C"
    \push-to-tag #'foo p1
    \push-to-tag #'foo p2
    \append-to-tag #'foo a1
    \append-to-tag #'foo a2
    \tagged

    "B C"
    \push-to-tag #'foo p
    \remove-with-tag #'foo
    \tagged

    "B C"
    \push-to-tag #'foo p
    \keep-with-tag #'bar
    \tagged
  }
}
