
\version "2.1.36"

\header {
texidoc="Ordinary slurs should work well with phrasing slur."
}

\score {
  \notes\relative c'' {
    \time 6/4 c\((d e) f(e d)\)
  }
  \paper {
    raggedright = ##t
  }
}

