% paper-as5.ly

\version "1.3.120";

paperAsFive = \paper {
	staffheight = 5.\char;

	\stylesheet #(as-make-style-sheet 'as5)
	
	\translator { \StaffContext barSize = \staffheight; }

	% no beam-slope
	%\translator { \VoiceContext beamHeight = #0; }
	\include "params-as.ly";
}

\paper { \paperAsFive }
