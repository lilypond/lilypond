% paper-as5-init.ly

\version "1.9.1"

paperAsFive = \paper {
	staffheight = 5.\char

	#(define fonts (as-make-font-list 'as5))
	
	\translator { \StaffContext barSize = #5 }

	% no beam-slope
	%\translator { \VoiceContext beamHeight = #0 }
	\include "params-as-init.ly"
}

\paper { \paperAsFive }
