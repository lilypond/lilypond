\header{
texidoc="
Beams should behave reasonably well, even under extreme circumstances.
Stems may be short, but noteheads should never touch the beam.
";
}
\version "1.3.110";
\score{
	\notes\relative c''{
		[g8 c c,]
		[c16 c'' a f]
		\stemUp 
		[c,,32 c'' a f]

	}
	\paper{
		linewidth=-1.;
	}
}
