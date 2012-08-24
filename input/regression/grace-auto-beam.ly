
\version "2.16.0"
\header
{
  texidoc = "The autobeamer is not confused by grace notes."
}
\layout { ragged-right = ##t }



\context Voice \relative c''{

  \grace a8 g16 f e f \grace a8 g16 f e f \grace a8 g16 f e f 
  \grace a8 g16 f e f |
}



