\version "2.7.13"
\header {
texidoc = "Single head notes may collide. "
}

\layout { raggedright= ##t }


\context Staff  \transpose c c' <<  
  {  c4 d e f g2 g4 a | }  \\
  { g4 f e g  g2 g2 } 
>>


