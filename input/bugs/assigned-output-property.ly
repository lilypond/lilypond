
%
% Using music-variable breaks output property
%

foo = \notes\relative c''{
    \outputproperty #(make-type-checker 'note-head-interface) 
	#'extra-offset = #'(2 . 3)
    c2
    c
}

\score{
    <
	\context Staff=a\notes\relative c''{
	    \outputproperty #(make-type-checker 'note-head-interface) 
		#'extra-offset = #'(2 . 3)
	    c2
	    c
	}
        \context Staff=b \foo
    >
    \paper{
	linewidth=-1.0;
	\translator {
	    \ScoreContext
	    \consists "Mark_engraver";
	}
    }
}
