\paper{
	\paper_sixteen
	textheight = 295.0\mm;
	linewidth = 180.0\mm;

	% slurs are never beautiful (no steep slurs)
	slur_beautiful = 0.0;

	\translator {
		\VoiceContext
		dynamicPadding = #2  % urg, in \pt
		dynamicMinimumSpace = #6  % urg, in \pt
	}
	\translator {
		\VoiceContext
		\name "VoiceOne";

		dynamicPadding = #2  % urg, in \pt
		dynamicMinimumSpace = #6  % urg, in \pt

		%%\consists "Line_number_engraver";
		verticalDirection = #1
		stemVerticalDirection = #1
		%dynamicDirection = #-1
		dynamicDirection = #1
	}
	\translator {
		\VoiceContext
		\name "VoiceTwo";
		%%\consists "Line_number_engraver";
		verticalDirection = #-1
		stemVerticalDirection = #-1
		\remove "Dynamic_engraver";
		%% Aargh: absulute dynamics:
		\remove "Text_engraver";
	}
	\translator { 
		\HaraKiriStaffContext 
		\accepts "VoiceOne";
		\accepts "VoiceTwo";
	}
	\translator { 
		\OrchestralScoreContext 
		%% URG: this changes dynamics too
		%%textStyle = #"italic"
		timeSignatureStyle = #"C"
      		instrumentScriptPadding = #55  %% urg, this is in pt
      		instrScriptPadding = #35 %% urg, this is in pt
		marginScriptHorizontalAlignment = #1
		maximumRestCount = #1
	}
}
