\paper {
	textheight = 295.0\mm;
	linewidth = 180.0\mm;

	% slurs are never beautiful (no steep slurs)
	slur_beautiful = 0.0;

	\translator {
		\VoiceContext
		%dynamicPadding = #5  % urg, in \pt
		%dynamicMinimumSpace = #10  % urg, in \pt
	}
	\translator {
		\VoiceContext
		\name "VoiceOne";
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
		\OrchestralPartStaffContext 
		\accepts "VoiceOne";
		\accepts "VoiceTwo";
	}
	\translator { 
		\ScoreContext skipBars = ##t 
		%% URG: this changes dynamics too
		%%textStyle = #"italic"
		timeSignatureStyle = #"C"
      		instrumentScriptPadding = #60  %% urg, this is in pt
      		instrScriptPadding = #40 %% urg, this is in pt
		marginScriptHorizontalAlignment = #1
		maximumRestCount = #1
	}
}
