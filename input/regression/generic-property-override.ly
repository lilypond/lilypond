\version "1.3.146"
\header{
texidoc="
More specific settings take precendence over less specific settings. The
second slur has slurDirection set to down, overriding the stemup  setting.
"
}


\score {
 \notes \relative c'' \context Voice {
	\stemUp
	c'4 () c4 
	\slurDown
	c4 ( )c4 
 }
 \paper { linewidth = -1.0 }
}
