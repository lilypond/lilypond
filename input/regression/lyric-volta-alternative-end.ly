\version "2.23.5"

\header {
  texidoc = "Lyrics can be structured using repeats with alternative
endings.  This case has a repeat that ends at the end of the score."
}

music = {
  \repeat volta 2 c'1 \alternative { d'1 e'1 }
  \addlyrics { \repeat volta 2 cee \alternative { dee eee } }
}

\score { \music }
\score { \unfoldRepeats \music }
