\version "1.0.7";

\score{
	\notes\transpose c'{
		\property Score.beamquantisation = \none

		[a'8 <a' g'']>
		[c <c e,]>
		[a'16 <a' g'']>
		[c <c e,]>
		[a'32 <a' g'']>
		[c <c e,]>
	}
	\paper{
		castingalgorithm = \Wordwrap;
		linewidth = 60.0\mm;
	}
}
