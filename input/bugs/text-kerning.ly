

\header {
texidoc= "LilyPond doesn't kern text, but TeX does, leading to skewed results: the VAVAVAVA takes less space than LilyPond thinks it does."; } 

\score {
 \notes \relative c'' < \context Voice {
%	 c16 c16 c16 c16 c4
	 c4 c4 c4
 }
\context Lyrics  \lyrics {   foobar4 -- VAVAVAVAV4 -- foobar4 } >

	
 \paper { linewidth = -10.0\cm; }
}
