\score{
    <
        \context Staff=one \notes\relative c''{
            a \glissando e'
	    % test: link thread to other staff
	    a, \glissando
	    \translator Staff=two
	    a,,
	}
	\context Staff=two { \clef bass; \skip 1; }
    >
    \paper{
        linewidth = 70.\mm;
    }
}