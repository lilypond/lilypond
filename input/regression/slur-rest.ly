
\version "2.8.0"
\header
{
  texidoc ="Slurs may be placed over rest. The slur will avoid colliding with
the rest.

"
}

\layout { ragged-right = ##t }
{ \stemDown c'4 ( r4  c'2)

  \relative c'' { 
    %% Finish with F if played separately 
    c8-.(   r c,-. r  c4) r4|
  }
  
}



