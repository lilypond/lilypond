
#(eval-string (ly-gulp-file "midi.scm"))

\midi {
        \tempo 4=60;
	\include "performer.ly"
%	unfold_all = "1";
}

