\version "1.0.14";

scales = \notes\transpose c''{
		f2 f f f f f f f f f\break 
	}

keys = \notes{
                \key b; s1 \key f; s1 \key f; s1 \key f; s1 \key f; s1 
	}

\score{
	<
		\type ChordNames \scales
		\type Staff < \scales \keys >
	>
}
