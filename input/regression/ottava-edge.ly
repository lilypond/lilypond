
\header
{
  texidoc = 
  "Both edge heights of an ottava bracket can be specified."
}

\version "2.17.6"


\layout { ragged-right = ##t} 

\relative c'''  {

  %standard ottavation
  \ottava #1
  a b c
  \ottava #0
  a
  
  %override the left edge height to produce standard text with a left edge
  \ottava #1
  \once \override Staff.OttavaBracket.edge-height = #'(1.2 . 1.2)
  a b c
  \ottava #0
  a
  
  % Look! we can make them go up!
  \ottava #1
  \once \override Staff.OttavaBracket.edge-height = #'(-1 . -1)
  a b c
  \ottava #0
  a
  
  % and have them go in different directions
  \ottava #1
  \once \override Staff.OttavaBracket.edge-height = #'(-1.2 . 1.2)
  a b c
  \ottava #0
  a
 
}


