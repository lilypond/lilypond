% paper16.ly



\version "1.1.52";

paper_sixteen = \paper {
	staffheight = 16.0\pt;

	% ugh, see table16 for sizes
	quartwidth = 5.28\pt;
	wholewidth = 7.92\pt;
	
	font_large = 12.;
	font_Large = 10.;
	font_normal = 8.;

	font_finger = 4.;
	font_volta = 5.;
	font_number = 8.;
	      font_dynamic = 10.;
   font_mark = 10.;

     	arithmetic_basicspace = 2.;
        arithmetic_multiplier = 4.8\pt;
	texsetting = "\\input lilyponddefs ";
	pssetting = "(lilyponddefs.ps) findlibfile {exch pop //systemdict /run get exec} { /undefinedfilename signalerror } ifelse\n";
	scmsetting = "\"(lilyponddefs.ps) findlibfile {exch pop //systemdict /run get exec} { /undefinedfilename signalerror } ifelse\";\n";
	scmsetting = "\"\\\\input lilyponddefs \\\\musixsixteendefs\";\n";

	0 = \font "feta16" 
	-1 = \font "feta13"
	-2 = \font "feta11"
	
	\include "params.ly";
}
