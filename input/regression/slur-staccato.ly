\version "1.5.68"
\header {
texidoc="Manual hack for slur and staccato."
}

\paper { linewidth = -1.0 }

\score {
  \context Staff \notes\relative c'' {
    \property Voice.Slur \override
      #'attachment-offset = #'((0 . 1) . (0 . 1))
    a-.( g-. )a-.
    \property Voice.Slur \override
      #'attachment-offset = #'((0 . 1.5) . (0 . 1.5))
    b-.( a-. )b-.
  }
}	