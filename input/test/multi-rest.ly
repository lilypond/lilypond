\version "1.2.0";

voice_one = \notes\transpose c''{  \stemup
	R1 * 2 | f'4-. r r2 | R1 * 3 |
	f'4-. r r2 | R1 * 3 |
	es'4-. r r2 | r1 |
	\property Score.skipBars = 1
	g4-. r r2 | g4-. r r2 |
	R1 * 5 |
	r2 r4 g4-. |
	}

voice_two = \notes
	{ \transpose c' { \stemdown
	R1 * 2 | f'4-. r r2 | R1 * 3 |
	f'4-. r r2 | R1 * 3 |
	es'4-. r r2 | r1 |
	\property Score.skipBars = 1
	g4-. r r2 | g4-. r r2 |
	R1 * 5 |
	r2 r4 g4-. |

	}}

\score {  \notes
	\context Staff <
		\$voice_two
		\$voice_one
		>

 }

