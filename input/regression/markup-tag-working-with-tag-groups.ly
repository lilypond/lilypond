\version "2.25.27"

\header {
  texidoc = "Markup tags work with @code{\\tagGroup}."
}

\tagGroup #'(a b)
\tagGroup #'(foo bar)

test = \markup {
  \tag #'a a
  \tag #'b b
  \tag #'(a b) ab
  \tag #'foo foo
  \tag #'bar bar
  \tag #'(foo bar) foobar
  \tag #'(a foo) afoo
  \tag #'(a bar) abar
  \tag #'(b foo) bfoo
  \tag #'(b bar) bbar
  \tag #'test test
}

\markuplist {
  \override #'(padding . 2)
  \table
  #'(-1 -1)
  {
    \bold { Expectation Result }

    % first line of each snippet is the expectation
    % and the second line produces the result with tags to be tested

    "a b ab foo bar foobar afoo abar bfoo bbar test"
    \test

    "a ab foo bar foobar afoo abar test"
    \keep-with-tag #'a \test

    "b ab foo bar foobar bfoo bbar test"
    \keep-with-tag #'b \test

    "a ab foo foobar afoo abar bfoo test"
    \keep-with-tag #'(a foo) \test

    "a ab foo foobar afoo test"
    \keep-with-tag #'a \keep-with-tag #'foo \test

    "ab foo foobar bfoo test"
    \keep-with-tag #'b \keep-with-tag #'(a foo) \test

    "ab foo bar foobar test"
    \keep-with-tag #'b \keep-with-tag #'a \test

    "a b ab foo bar foobar afoo abar bfoo bbar"
    \keep-with-tag #'other \test
  }
}
