% paper20.ly


\version "1.0.14";

paper_twenty = \paper {
	staffheight = 20.0\pt;

	font_large = 12.;
	font_Large = 12.;	
	font_normal = 10.;

	font_finger = 5.;
	font_volta = 8.;
	font_number = 10.;
	font_mark = 12.;

	% what about:
	"font_number-1" = 8.;
	%"font_number" = 10.;
	"font_number+1" = 12.;
	
	% Ugh
	magnification_dynamic = 2.;
	
	% ugh see table20 for sizes
	quartwidth =  6.61\pt;
	wholewidth = 9.90\pt;


	arithmetic_basicspace = 2.;
        arithmetic_multiplier = 6.\pt;
	texsetting = "\\input lilyponddefs \\musixtwentydefs ";
	pssetting = "(lilyponddefs.ps) findlibfile {exch pop //systemdict /run get exec} { /undefinedfilename signalerror } ifelse\n";
	% urg, debugging only
	scmsetting = "(display \"(lilyponddefs.ps) findlibfile {exch pop //systemdict /run get exec} { /undefinedfilename signalerror } ifelse\");\n";
	scmsetting = "(display \"\\\\input lilyponddefs \\\\musixtwentydefs\");\n";

	-2 = \font "feta13"
	-1 = \font "feta16"
	0 = \font "feta20"

	\include "params.ly";
}

