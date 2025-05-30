\version "2.25.27"

\header {
  texidoc = "Tag filters like @code{\\keepWithTag} keep melismata in lyrics intact."
}

music = \relative c'' { c8 c4 b8 a4 r }

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
      \music
      \addlyrics { foo -- _ _ bar }
    }
    \score {
      \keepWithTag #'foo {
        \music
        \addlyrics { foo -- _ _ bar }
      }
    }

    \score {
      \music
      \addlyrics { foo -- _ _ bar }
    }
    \score {
      \keepWithTag #'foo {
        \music
        \addlyrics { \markup { \tag #'test test foo } -- _ _ bar }
      }
    }
  }
}
