
\version "2.3.4"

\header {
texidoc="Ordinary slurs should work well with phrasing slur."
}

\score {
  \relative c'' {
    \time 6/4 c\((d e) f(e d)\)
  }
  \paper {
    raggedright = ##t
  }
}

