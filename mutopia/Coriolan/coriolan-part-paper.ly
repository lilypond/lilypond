\paper {
	\translator { 
		\OrchestralPartStaffContext 
      		marginScriptPadding = #10  %% urg, this is in pt
	}
	\translator {
		\VoiceContext
		\name "VoiceOne";
		\consists "Line_number_engraver";
		verticalDirection = #1
		stemVerticalDirection = #1
		dynamicDirection = #-1
	}
	\translator {
		\VoiceContext
		\name "VoiceTwo";
		\consists "Line_number_engraver";
		verticalDirection = #-1
		stemVerticalDirection = #-1
		\remove "Dynamic_engraver";
		%% Aargh: absulute dynamics:
		\remove "Text_engraver";
	}

	\translator {
		\StaffContext
		\accepts "VoiceOne";
		\accepts "VoiceTwo";
	}
	\translator { \ScoreContext skipBars = ##t }
}
