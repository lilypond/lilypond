\version "2.25.27"

\header {
  texidoc = "Test @code{\\keep-with-tag} and @code{\\remove-with-tag}
on tagged lists of markups."
}

tagged-list-as-markup = \markup { pre \tag #'foo { A B } post }

tagged-list-as-markup-list = \markuplist {
  above \tag #'foo { A B } below
}

\markuplist {
  \override #'(padding . 2)
  \table
  #'(-1 -1)
  {
    \bold { Expectation Result }

    % first line of each snippet is the expectation
    % and the second line produces the result with tags to be tested

    "pre A B post"
    \keep-with-tag #'foo \tagged-list-as-markup

    "pre A B post"
    \keep-with-tag #'(foo bar) \tagged-list-as-markup

    "pre post"
    \remove-with-tag #'foo \tagged-list-as-markup

    "pre post"
    \remove-with-tag #'(foo bar) \tagged-list-as-markup

    \box \column { above A B below }
    \box \column \tagged-list-as-markup-list

    \box \column { above below }
    \box \column \remove-with-tag #'foo \tagged-list-as-markup-list

    "A"
    \first-visible \keep-with-tag #'foo { \tag #'foo { A B } C }

    "C"
    \first-visible \remove-with-tag #'foo { \tag #'foo { A B } C }

    "C"
    \remove-with-tag #'foo { \tag-list #'foo { A B } C }
  }
}
