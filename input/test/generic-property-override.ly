\version "1.3.93";

\score {
 \notes \relative c'' \context Voice {
	\stemUp
	c'4 () c4 
	\slurDown
	c4 ( )c4 
 }
 \paper { linewidth = -1.0; }
}
