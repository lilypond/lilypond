\header {
    texidoc = "Lyrics are not lowered despite the presence of
a clef transposition (8 below the clef)."
}

\version "2.16.0"

\paper {
  ragged-right = ##t
}

\relative c' { \clef "G_8" c c c c }
\addlyrics { bla bla bla bla }
