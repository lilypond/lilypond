% followThread: connect note heads with line when thread switches staff 

\score{
    <
        \context Staff=one \notes\relative c''{
	    \context Thread
            a
	    \translator Staff=two
	    a,, a
	    \translator Staff=one
	    a''
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