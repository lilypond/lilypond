\version "1.3.4";

\score{
	\notes\transpose c'{
		\property Score.beamQuantisation = #'none

		[a'8 <a' g''>]
		[c <c e,>]
		[a'16 <a' g''>]
		[c <c e,>]
		[a'32 <a' g''>]
		[c <c e,>]
	}
	\paper{
		castingalgorithm = \Wordwrap;
		linewidth = 60.0\mm;
	}
}
