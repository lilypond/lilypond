\header{
texidoc="
Multiple measure rests do not collide with barlines and clefs.  They
are not expanded when you set @code{Score.skipBars}.  Although the
multi-measure-rest is a Spanner, minimum distances are set to keep it
colliding from barlines. 
";
}
\version "1.3.117";

\score { \notes { \time 3/4; \key cis \major;
	    R2.*15 R2. R2.*7 }
	\paper {
	\translator {
		\ScoreContext
		skipBars = ##t
	}
	linewidth = -1.;	
	}
}
