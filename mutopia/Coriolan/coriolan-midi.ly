\midi{ 
	\tempo 4 = 150; 
	\translator {
		\VoiceContext
		\name "VoiceOne";
	}
	\translator {
		\VoiceContext
		\name "VoiceTwo";
		%\remove "Span_dynamic_engraver";
		%\remove "Dynamic_engraver";
	}
	\translator {
		\StaffContext
		\accepts "VoiceOne";
		\accepts "VoiceTwo";
	}
}
