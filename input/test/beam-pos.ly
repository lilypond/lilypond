\version "1.0.7";

\score{
	\notes\transpose c''{
		\property Score.beamquantisation = 3 % TEST

		[c8 c] [c c] [c c] [c c]
		[a' a'] [a' a'] [a' a'] [a' a']
		[c16 c] [c c] [c c] [c c]
		[a' a'] [a' a'] [a' a'] [a' a']
	}
	\paper{
		castingalgorithm = \Wordwrap;
		linewidth = 60.0\mm;
	}
}

