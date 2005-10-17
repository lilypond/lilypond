
\version "2.7.13"
\header
{
  texidoc = "The autobeamer is not confused by grace notes."
}
\layout { raggedright = ##t }



\context Voice \relative c''{

  \grace a8 g16 f e f \grace a8 g16 f e f \grace a8 g16 f e f 
  \grace a8 g16 f e f |
}



