
\version "2.12.0"
\header
{
  texidoc ="Slurs may be placed over rests. The slur will avoid
colliding with the rests.

"
}

\layout { ragged-right = ##t }
{ \stemDown c'4 ( r4  c'2)

  \relative c'' { 
    %% Finish with F if played separately 
    c8-.(   r c,-. r  c4) r4|
  }
  
}



