\paper {
	% slurs are never beautiful (no steep slurs)
	slur_beautiful = 0.0;

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
      		instrumentScriptPadding = #55  %% urg, this is in pt
      		instrScriptPadding = #25  %% urg, this is in pt
		maximumRestCount = #1
	}
	\translator { \OrchestralPartStaffContext }
	\translator { \ScoreContext skipBars = ##t }
}
