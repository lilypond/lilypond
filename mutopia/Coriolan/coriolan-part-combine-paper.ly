\paper {
	textheight = 295.0\mm;
	linewidth = 180.0\mm;

	% slurs are never beautiful (no steep slurs)
	slur_beautiful = 0.0;

	\translator {
		\ThreadContext
		\consists Rest_engraver;
	}
	\translator {
		\VoiceContext
		\remove Rest_engraver;
		\remove Slur_engraver;
		\remove Tie_engraver;
	}
	\translator {
		\StaffContext
%		\consists Slur_engraver;
		\consists Tie_engraver;
	}
	\translator { 
		\ScoreContext skipBars = ##t 
		%% urg: in pt?
		barNumberScriptPadding = #15
		%% URG: this changes dynamics too
		%%textStyle = #"italic"
		timeSignatureStyle = #"C"
		instrumentScriptPadding = #60  %% urg, this is in pt
		instrScriptPadding = #40 %% urg, this is in pt
		marginScriptHorizontalAlignment = #1
		maximumRestCount = #1
	}
}

