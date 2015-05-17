\header {
    texidoc = "Lyrics are not lowered despite the presence of
a clef transposition (8 below the clef)."
}

\version "2.19.21"

\paper {
  ragged-right = ##t
}

\relative { \clef "G_8" c' c c c }
\addlyrics { bla bla bla bla }
