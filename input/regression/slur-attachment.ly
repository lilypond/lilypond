
\version "2.1.36"

\header{
texidoc="
Slurs should be attached to note heads, except when they would collide
with beams.  
"
}
\score{
	\notes \relative c''{
	        \override Slur  #'direction = #1
	        a8( a a4)
		a4( a8 a)
	        a8 a(a4)
		a4( a8) a
	}
	\paper{ 
		indent = 0.0
		linewidth = 100.\mm
	}
}

