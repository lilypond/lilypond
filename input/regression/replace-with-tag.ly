\version "2.25.32"

\header {
  texidoc = "Tagged music should be replaced using @code{\\replaceWithTag}."
}

music = {
  c'^\markup { \tag #'foo foo }
  d'^\markup { \tag #'bar bar }
  \tag #'baz e'
  f'
  \tag #'multi { g' a' }
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

    % nothing is replaced here, because foo is a markup tag,
    % but we do not use \replaceWithTagMarkup here
    \score {
      {
        c'^"foo"
        d'^"bar"
        e'
        f'
        g'
        a'
      }
    }
    \score { \replaceWithTag #'foo f \music }

    % baz is a music tag and the e' is replaced with c d
    \score {
      {
        c'^"foo"
        d'^"bar"
        c d
        f'
        g'
        a'
      }
    }
    \score { \replaceWithTag #'baz { c d } \music }

    % the g' a' music part tagged with multi is replaced with a c
    \score {
      {
        c'^"foo"
        d'^"bar"
        e'
        f'
        c
      }
    }
    \score { \replaceWithTag #'multi c \music }

    % after the replacement with a c, the second replacement with a d
    % should not do anything, because the tag is gone.
    \score {
      {
        c'^"foo"
        d'^"bar"
        e'
        f'
        c
      }
    }
    \score {
      \replaceWithTag #'multi d
      \replaceWithTag #'multi c
      \music
    }

    % foo is a markup tag and so it should be replaced with new
    \score {
      {
        c'^"new"
        d'^"bar"
        e'
        f'
        g'
        a'
      }
    }
    \score {
      \replaceWithTagMarkup #'foo \markup { new } \music
    }

    % every matching tag should be replaced, so we try it with two
    \score {
      {
        f'
        d'
        f'
      }
    }
    \score {
      \replaceWithTag #'test f'
      {
        \tag #'test { c' }
        d'
        \tag #'test e'
      }
    }

    % the keepWithTag filter does not filter baz after replacement
    \score {
      {
        c'
        d'
        c d
        f'
        g'
        a'
      }
    }
    \score {
      \keepWithTag #'multi
      \replaceWithTag #'baz { c d }
      \music
    }

    % if we filter first, the replacement has no effect
    \score {
      {
        c'
        d'
        f'
        g'
        a'
      }
    }
    \score {
      \replaceWithTag #'baz { c d }
      \keepWithTag #'multi
      \music
    }

    % we should be able to replace something with itself
    % (containing the same tags)
    \score {
      {
        c'^"foo"
        d'^"bar"
        c'^"foo"
        d'^"bar"
        e'
        f'
        g'
        a'
        f'
        g'
        a'
      }
    }
    \score { \replaceWithTag #'baz \music \music }
  }
}
