\version "2.25.27"

\header {
  texidoc = "Markup tags get filtered by @code{\\keepWithTag}
and @code{\\removeWithTag}."
}

music = {
  c'^\markup { \tag #'foo foo }
  d'^\markup { \tag #'bar bar }
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

    \score {
      {
        c'^"foo"
        d'
      }
    }
    \score { \keepWithTag #'foo \music }

    \score {
      {
        c'
        d'^"bar"
      }
    }
    \score { \removeWithTag #'foo \music }

    \score {
      {
        c'^"pre foo"
        d'^"bar"
      }
    }
    \score { \pushToTagMarkup #'foo \markup { pre } \music }

    \score {
      {
        c'^"p1 p2 foo"
        d'^"bar"
      }
    }
    \score {
      \pushToTagMarkup #'foo \markup p1
      \pushToTagMarkup #'foo \markup p2
      \music
    }

    \score {
      {
        c'^"foo"
        d'^"bar post"
      }
    }
    \score {
      \appendToTagMarkup #'bar \markup { post } \music
    }
  }
}
