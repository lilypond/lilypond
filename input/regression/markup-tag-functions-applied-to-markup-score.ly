\version "2.25.27"

\header {
  texidoc = "Markup tags get applied to @code{\\score}-markup."
}

music = {
  \tag #'foo c'
  \tag #'bar d'
  e'^\markup { test \tag #'foo foo }
}

\markuplist {
  \override #'(padding . 2)
  \override #'(baseline-skip . 13)
  \table
  #'(-1 -1)
  {
    \bold { Expectation Result }

    % first line of each snippet is the expectation
    % and the second line produces the result with tags to be tested

    \score{
      {
        c'
        e'^"test foo"
      }
    }
    \keep-with-tag #'foo \score { \music }

    \score{
      {
        d'
        e'^"test"
      }
    }
    \remove-with-tag #'foo \score { \music }

    \score{
      {
        c'
        d'
        e'^"test p1 p2 foo a2 a1"
      }
    }
    \push-to-tag #'foo p1
    \push-to-tag #'foo p2
    \append-to-tag #'foo a1
    \append-to-tag #'foo a2
    \score { \music }
  }
}