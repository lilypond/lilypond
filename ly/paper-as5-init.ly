% paper-as5-init.ly

\version "1.5.49"

paperAsFive = \paper {
	staffheight = 5.\char

	\stylesheet #(as-make-style-sheet 'as5)
	
	\translator { \StaffContext barSize = #5 }

	% no beam-slope
	%\translator { \VoiceContext beamHeight = #0 }
	\include "params-as-init.ly"
}

\paper { \paperAsFive }
