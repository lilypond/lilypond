\version "1.5.68"

\header {
texidoc="Slurs play well with phrasing slur."
}

\score {
  \notes\relative c'' {
    \time 6/4 c\((d)e f(e)\)d
  }
  \paper {
    linewidth = -1.
  }
}
