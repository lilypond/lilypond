
\version "2.3.22"
\header
{
    texidoc = "The autobeamer is not confused by grace notes."
}

\score{
  \context Voice \relative c''{

    \grace a8 g16 f e f \grace a8 g16 f e f \grace a8 g16 f e f 
        \grace a8 g16 f e f |
  }
  \layout { raggedright = ##t }
}

