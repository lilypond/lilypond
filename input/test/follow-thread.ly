% followThread: connect note heads with line when thread switches staff 

\score{
    \context PianoStaff <
        \context Staff=one \notes\relative c''{
	    \context Thread
            a
	    \translator Staff=two
	    a,

% smaller = easier to debug.
%{	a
	    \translator Staff=one
	    a''
%}
		s2
	}
	\context Staff=two { \clef bass; \skip 1; }
    >
    \paper{
        linewidth = 70.\mm;
	\translator {
	    \ScoreContext
	    followThread = ##t
	}
    }
}
