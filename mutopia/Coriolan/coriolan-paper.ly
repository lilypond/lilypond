\paper{
	\paper_sixteen
	% slurs are never beautiful (no steep slurs)
	slur_beautiful = 0.0;

	%%
	%%\translator { \VoiceContext \remove "Dynamic_engraver"; }

	\translator {
		\VoiceContext
		\name "VoiceOne";

		%%
		%%\remove "Dynamic_engraver";

		%%\consists "Line_number_engraver";
		verticalDirection = #1
		stemVerticalDirection = #1
		dynamicDirection = #-1
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
      		instrumentScriptPadding = #55  %% urg, this is in pt
      		instrScriptPadding = #25 %% urg, this is in pt
		\accepts "VoiceOne";
		\accepts "VoiceTwo";
	}
	\translator { \OrchestralScoreContext }
}
