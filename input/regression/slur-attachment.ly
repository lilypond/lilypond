
\version "2.3.22"

\header{
texidoc="
Slurs should be attached to note heads, except when they would collide
with beams.  
"
}
\score{
	 \relative c''{
	        \override Slur  #'direction = #1
	        a8( a a4)
		a4( a8 a)
	        a8 a(a4)
		a4( a8) a
	}
	\layout{ 
		indent = 0.0
		linewidth = 100.\mm
	}
}

