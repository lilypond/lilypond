\version "2.25.27"

\header {
  texidoc = "Test nesting tags in markups and arguments of @code{\\push-to-tag} and @code{append-to-tag}."
}

\markuplist {
  \override #'(padding . 2)
  \table
  #'(-1 -1)
  {
    \bold { Expectation Result }

    % first line of each snippet is the expectation
    % and the second line produces the result with tags to be tested

    "A a p B a C"
    \append-to-tag #'foo a
    \push-to-tag #'bar p
    \line { \tag #'foo { A \tag #'bar B } C }

    "A a"
    \append-to-tag #'foo \tag #'foo a
    \line { \tag #'foo A }

    "B"
    \keep-with-tag #'foo
    \remove-with-tag #'bar
    \line { \tag #'(foo bar) A B }

    "B"
    \remove-with-tag #'bar
    \keep-with-tag #'foo
    \line { \tag #'(foo bar) A B }

    "pre a b A B"
    \push-to-tag #'bar pre
    \push-to-tag #'foo \line { \tag #'bar a b }
    \line { \tag #'foo A B }
  }
}
