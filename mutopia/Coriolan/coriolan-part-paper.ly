\version "1.3.120"
\paper {
	textheight = 295.0\mm;
	linewidth = 180.0\mm;

	\translator{ \HaraKiriStaffContext }

	\translator {
		\ScoreContext
		%\OrchestralScoreContext
		skipBars = ##t 
		TimeSignature \override #'style = #'C
		BarNumber \override #'padding = #3
		RestCollision \override #'maximum-rest-count = #1
	}
}
