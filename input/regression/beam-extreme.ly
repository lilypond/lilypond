\header{
texidoc="
Beams should behave reasonably well, even under extreme circumstances.
Stems may be short, but noteheads should never touch the beam.  Note that
under normal circumstances, these beams would get knees; here
Beam.auto-knee-gap was set to false.
";
}

\score{
	\notes\relative c''{
		[g8 c c,]
		[c16 c'' a f]
		\stemUp 
		[c,,32 c'' a f]

	}
	\paper{
		linewidth=-1.;
		\translator {
		        \VoiceContext
			% If we want to test extreme beams,
			% we should not have them auto-kneed
			Beam \override #'auto-knee-gap = ##f
		}
	}
}
