\paper {
	texsetting = "\\input lilyponddefs ";
	pssetting = "(lilyponddefs.ps) findlibfile {exch pop //systemdict /run get exec} { /undefinedfilename signalerror } ifelse\n";
	scmsetting = "\"(lilyponddefs.ps) findlibfile {exch pop //systemdict /run get exec} { /undefinedfilename signalerror } ifelse\";\n";
}
