\version "2.25.32"

\header {
  texidoc = "Test multiple use of @code{\\replace-with-tag} on markups."
}

tagged = \markup {
  \tag #'(foo bar) foobar
  test
}

\markuplist {
  \override #'(padding . 2)
  \table
  #'(-1 -1)
  {
    \bold { Expectation Result }

    % first line of each snippet is the expectation
    % and the second line produces the result with tags to be tested


    % tag in replacement is kept, and can be
    % replaced with "baz".
    "baz test test"
    \replace-with-tag #'foo "baz"
    \replace-with-tag #'foo \tagged \tagged

    % replace with b so second replacement does nothing
    "b test"
    \replace-with-tag #'foo "f"
    \replace-with-tag #'bar "b"
    \tagged

    % replace with f so second replacement does nothing
    "f test"
    \replace-with-tag #'bar "b"
    \replace-with-tag #'foo "f"
    \tagged
  }
}
