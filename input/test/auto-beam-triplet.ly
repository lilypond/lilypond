
\score{
	\notes\relative c''{
		c8 c c c
		\times 2/3 { c c c c c c}
	}
	\paper{
		% urg, avoid crash
		\translator{
			\VoiceContext
			\remove Tuplet_engraver;
		}
	}
}
