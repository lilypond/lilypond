\header{
texidoc="
Simple glissando lines between notes are supported.
";
}

\score{
    <
        \context Staff=one \notes\relative c''{
            a \glissando e' \glissando a, \glissando
	    \translator Staff=two
	    a,,
	}
	\context Staff=two { \clef bass; \skip 1; }
    >
    \paper{
        linewidth = 70.\mm;
    }
}