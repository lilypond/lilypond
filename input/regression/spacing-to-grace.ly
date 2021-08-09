
\version "2.19.21"
\header {

  texidoc ="Space from a normal note (or bar line) to a grace note is
    smaller than to a normal note."
  
}

\layout { ragged-right = ##t}


\context Voice 
{ \time 2/4
  \relative {
    e''8 e \grace d8 e e \grace f8 e es, d' d
    e8 e e e \grace { \stemDown f8 \stemNeutral } e es, d'

  }  

}



