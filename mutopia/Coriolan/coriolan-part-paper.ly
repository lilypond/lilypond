\paper {
	textheight = 295.0\mm;
	linewidth = 180.0\mm;

	\translator{ \HaraKiriStaffContext }

	\translator {
		\ScoreContext
		skipBars = ##t 

		barScriptPadding = #2.0 % dimension \pt
		markScriptPadding = #4.0
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
