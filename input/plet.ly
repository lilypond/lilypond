
melanie = \melodic{
	\meter{2/4}
	\duration{8}
	[ c d ] { [ d d' } { c c' ] } |
	[ c d ] [ e { d g } c ]2/3 |
	[ c d ] [ c16 d16 e'16 e'16 d16 c16 ]4/6 |
	[ c d e e d c ]4/6 |
	[c d ] < { [ c d e ]2/3 } { [ f g a ]2/3 } > |
	[ c d ] [ d c ] |
        
}

michelle = \melodic{
	\meter{2/4}
	\duration{8}
	[ c c c c ] |
	[ c c c c ] |
	[ c c c c ] |
	[ c c c c ] |
	[ c c c c ] |
	[ c c c c ] |
}

mireille = \lyric{
	\meter{2/4}
	\duration{8}
	o o o o |
	o o [ o o o ]2/3 |
	[ o o o ]2/3 [ o16 o16 o16 o16 o16 o16 ]4/6 |
	[ o o o o o o]4/6 |
	o o [ o o o ]2/3 |
	o o o o |
}

\score{
	\staff{ mireille }
	\staff{ melanie }
	\staff{ michelle }
	\paper{}
}
