\version "2.19.21"

\header {
  texidoc = "The control points of a broken or unbroken slur may be offset by
@code{\\shape}.  The blue slurs are modified from the default slurs shown first."
}

\layout {
  indent = 0
  ragged-right = ##t
}

% unmodified
\relative {
  d''4( d' b g
  g,8 f' e d c2)
  d4( d' b g
  \break
  g,8 f' e d c2)
}

% modified
\relative c'' {
  \override Slur.color = #blue
  \shape #'((0 . -2) (-1 . 3.5) (0.5 . 0.5) (0 . -2.5)) Slur
  d4( d' b g g,8  f' e d c2)
  \shape #'(
   ((0 . -2.5) (0 . 1.5) (0 . 1) (0 . -0.5))
   ((1 . 2.5) (0 . 1.5) (0 . 1) (0 . 0))
  ) Slur
  d4( d' b g
  \break
  g,8 f' e d c2)
}
