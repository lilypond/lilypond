\header{
texidoc="
Theads can be traced automagically when they switch staffs by setting
property @code{followThread}.
";
}
% followThread: connect note heads with line when thread switches staff 

\score{
    \context PianoStaff <
        \context Staff=one \notes\relative c''{
	    \context Thread	    
            d,
	    \translator Staff=two
	    c
%{
	    b
	    \translator Staff=one
	    a'

	    [c,8
	    \translator Staff=two
	    c]
	    s2.
%}
	}
	\context Staff=two { \clef bass; \skip 1*2; }
    >
    \paper{
        linewidth = 90.\mm;
	\translator {
	    \ScoreContext
	    followThread = ##t
	}
    }
}
