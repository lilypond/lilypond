\version "1.3.5";

\score{
	\notes\transpose c''{
		\property Score.beamQuantisation = 'test

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

