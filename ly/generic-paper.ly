
#(eval-string (ly-gulp-file "paper.scm"))

\paper {
	texsetting = "";
	pssetting = "";
	scmsetting = "(lilyponddefs.ps) findlibfile {exch pop //systemdict /run get exec} { /undefinedfilename signalerror } ifelse;\n";% UGH. 

}
