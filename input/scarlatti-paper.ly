
\paper{
	%my standard paper block


%	0=\font "feta19"
%	-1=\font "feta16"
%	\stylesheet #(make-style-sheet 'paper19)

	indent=0.;
	linewidth=188.\mm;

	\translator{
		\PianoStaffContext
		VerticalAlignment \set #'forced-distance = #8.4
	}
	\translator{\StaffContext
		TimeSignature \override #'style = #'C
	}
	\translator {
	  \ScoreContext
	  SpacingSpanner \override #'arithmetic-multiplier = #1.4
	}
	\translator{
		\VoiceContext
		noStemExtend = ##t
		tupletVisibility = ##f
		Stem \override #'stem-shorten = #'(0.0)
	}
}


