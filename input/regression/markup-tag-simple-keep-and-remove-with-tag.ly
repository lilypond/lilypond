\version "2.25.27"

\header {
  texidoc = "Test simple @code{\\keep-with-tag} and @code{\\remove-with-tag} on markups."
}

tagged = \markup {
  \tag #'foo { foo }
  \tag #'bar { bar }
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

    "foo bar test"
    \tagged

    "foo test"
    \keep-with-tag #'foo \tagged

    "bar test"
    \keep-with-tag #'bar \tagged

    "test"
    \keep-with-tag #'baz \tagged

    "test"
    \first-visible {
      \keep-with-tag #'baz
      \tagged
    }

    "foo bar test"
    \keep-with-tag #'(foo bar) \tagged

    "bar test"
    \remove-with-tag #'foo \tagged

    "test"
    \remove-with-tag #'(foo bar) \tagged

    "test"
    \keep-with-tag #'foo
    \remove-with-tag #'foo
    \tagged

    "bar test"
    \keep-with-tag #'(foo bar)
    \remove-with-tag #'foo
    \tagged

    "test"
    \keep-with-tag #'foo
    \keep-with-tag #'bar
    \tagged

    "foo test"
    #(markup->string
      (markup #:keep-with-tag 'foo tagged))

    "bar test"
    #(markup->string
      (markup #:remove-with-tag 'foo tagged))
  }
}
