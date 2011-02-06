\header {
    texidoc = "Lyrics are not lowered despite the presence of an octavation 8."
}

\version "2.12.0"

\paper {
  ragged-right = ##t
}

\relative c' { \clef "G_8" c c c c }
\addlyrics { bla bla bla bla }
