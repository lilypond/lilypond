% paper-as5.ly

\version "1.3.120";

paperAsFive = \paper {
	staffheight = 5.\char;

	font_large = 1.;
	font_Large = 1.;	
	font_normal = 1.;
	font_script = 1.;

	font_finger = 1.;
	font_volta = 1.;
	font_number = 1.;
	font_mark = 1.;

	% what about:
	"font_number-1" = 1.;
	%"font_number" = 10.;
	"font_number+1" = 1.;
	
	% Ugh
	magnification_dynamic = 2.;
	

	-2 = \font "as5"
	-1 = \font "as5"
	0 = \font "as5"

	"font_feta-2" = 9.;
	"font_feta-1" = 9.;
	"font_feta" = 9.;

	\include "params-as.ly";
}

\paper { \paperAs_five }
