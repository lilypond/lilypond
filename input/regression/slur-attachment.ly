
\header{
texidoc="
Slurs should be attached to note heads, except when they would collide
with beams.  Also see: ophee-slurs.
";
}
\score{
	\notes \relative c''{
	        \property Voice.Slur \set #'direction = #1
	        a8( a )a4
		a4( a8 )a
	        a8 a()a4
		a4() a8 a
	}
	\paper{ 
		indent = 0.0;
		linewidth = 100.\mm;
	}
}
