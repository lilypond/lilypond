\version "1.9.0"
\header {
texidoc = "Collisions for single head notes. "
}
    \paper { raggedright= ##t }


\score {
    \notes

  \context Staff \notes \transpose c c' <  
	{  c4 d e f g2 g4 a | }  \\
	{ g4 f e g  g2 g2 } 
  >
}


