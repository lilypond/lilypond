\version "1.3.93";

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
