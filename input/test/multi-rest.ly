\version "1.0.7";

voice_one = \notes\transpose c'	{  \stemup
	R1 * 2 | f'4-. r r2 | R1 * 3 |
	f'4-. r r2 | R1 * 3 |
	es'4-. r r2 | r1 |
	\property Score.SkipBars = 1
	g4-. r r2 | g4-. r r2 |
	R1 * 5 |
	r2 r4 g4-. |
	}

voice_two = \notes
	{ \transpose c, { \stemdown
	R1 * 2 | f'4-. r r2 | R1 * 3 |
	f'4-. r r2 | R1 * 3 |
	es'4-. r r2 | r1 |
	\property Score.SkipBars = 1
	g4-. r r2 | g4-. r r2 |
	R1 * 5 |
	r2 r4 g4-. |

	}}

\score {  \notes
	\type Staff <
		\$voice_two
		\$voice_one
		>

 }

