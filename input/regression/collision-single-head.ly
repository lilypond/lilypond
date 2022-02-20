\version "2.16.0"
\header {
texidoc = "Single head notes may collide. "
}

\layout { ragged-right= ##t }


\context Staff  \transpose c c' <<  
  {  c4 d e f g2 g4 a | }  \\
  { g4 f e g  g2 g2 } 
>>


