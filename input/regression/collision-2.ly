\version "2.1.28"
\header {
texidoc = "Single head notes may collide. "
}
    \paper { raggedright= ##t }


\score {
    \notes

  \context Staff \notes \transpose c c' <<  
	{  c4 d e f g2 g4 a | }  \\
	{ g4 f e g  g2 g2 } 
  >>
}


