\header {
texidoc = "Percent repeats are not skipped, even when skipBars is set.  "
}


\include "paper20.ly"

\score {
     \context Staff <
	\property Score.skipBars = ##t
	\notes {
	\repeat "percent" 2 { g2 a g a }
	}
     >
}

