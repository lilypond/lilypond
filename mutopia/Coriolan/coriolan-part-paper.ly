\paper {
	\translator { \OrchestralPartStaffContext }
	\translator { 
		\VoiceContext
%		noAutoBeam = ##t 
		\consists "Line_number_engraver";
	}
	\translator { \ScoreContext skipBars = ##t }
}
