\version "1.0.16";

scales = \notes\transpose c''{
		f2 f f f f f f f f f\break 
	}

keys = \notes{
                \key b; s1 \key f; s1 \key f; s1 \key f; s1 \key f; s1 
	}

\score{
	<
		\context ChordNames \scales
		\context Staff < \scales \keys >
	>
}
