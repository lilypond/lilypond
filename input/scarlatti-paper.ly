\version "1.3.146"

\paper{
	indent=0.
	linewidth=188.\mm

	\translator{
		\PianoStaffContext
		VerticalAlignment \override #'forced-distance = #8.4
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


