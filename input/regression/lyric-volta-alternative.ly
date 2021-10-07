\version "2.23.5"

\header {
  texidoc = "Lyrics can be structured using repeats with alternative
endings.  This case has a repeat that ends before the end of the
score.  The volta bracket ends before the rest."
}

music = {
  { \repeat volta 2 c'1 \alternative { d'1 e'1 } r2 f'2 }
  \addlyrics {
    \repeat volta 2 cee \alternative { dee eee } eff
  }
}

\score { \music }
\score { \unfoldRepeats \music }
