\version "2.25.32"

\header {
  texidoc = "Test simple @code{\\replace-with-tag} on markups."
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

    % just the output unmodified
    "foo bar test"
    \tagged

    % simple replacement of one tag
    "baz bar test"
    \replace-with-tag #'foo baz \tagged

    % second replacement does nothing,
    % because the tag was replaced, too.
    "baz bar test"
    \replace-with-tag #'foo baf
    \replace-with-tag #'foo baz
    \tagged

    % foo is replaced with nothing,
    % so it is not visible.
    % \replace-with-tag is a markup-command
    % and not a markup list so there is
    % the combined markup returned.
    "bar test"
    \first-visible {
      \replace-with-tag #'foo "" \tagged
    }

    % replacement contains same tags,
    % but should not end up in infinite loop.
    "foo bar test bar test"
    \replace-with-tag #'foo \tagged \tagged

    % markup->string should work as expected
    "foo baz test"
    #(markup->string
      (markup #:replace-with-tag 'bar "baz" tagged))

    "bar test"
    #(markup->string
      (markup #:replace-with-tag 'foo "" tagged))
  }
}
