\version "2.25.33"

\header {
  texidoc = "Tag filters like @code{\\keepWithTag}
filter lyrics and stanza markups."
}

music = \relative c'' { c8 c4 b8 a4 r }
taggedLyrics = \lyricmode {
  \set stanza = \markup {
    \tag #'first first
    \tag #'last last
  }
  foo
  \tag #'bar bar
  \tag #'baz baz
  \markup {
    \column {
      \tag #'bar "b"
      \tag #'baz "z"
    }
  }
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
      \music
      \addlyrics { \set stanza = "first" foo bar b }
    }
    \score {
      \keepWithTag #'(first bar) {
        \music
        \addlyrics { \taggedLyrics }
      }
    }

  }
}
