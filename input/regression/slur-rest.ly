
\version "2.19.21"
\header
{
  texidoc ="Slurs may be placed over rests. The slur will avoid
colliding with the rests.

"
}

\layout { ragged-right = ##t }
{ \stemDown c'4 ( r4  c'2)

  \relative { 
    %% Finish with F if played separately 
    c''8-.(   r c,-. r  c4) r4|
  }
  
}



