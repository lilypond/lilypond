\version "2.23.5"

\header {
  texidoc = "Lyrics can be structured using repeats and @code{\\fine}.
In the folded output, @emph{Fine} should appear at the end of the
first measure."
}

music = {
  \repeat volta 2 {
    c'1
    \volta 2 \fine
    \volta 1 { r1 d'1 }
  }
  \addlyrics {
    \repeat volta 2 {
      cee
      \volta 2 \fine
      \volta 1 dee
    }
  }
}

\score { \music }
\score { \unfoldRepeats \music }
