
\version "1.0.7";
\score{
	\notes \transpose c'''{
		\stemup
%		\stemdown
		% default abbreviations
		c4 c4: c4:32 c4: c4 c2.
		a,1
		a,1:32
		c,4:8 c': c,4:16 c':
		[ c,8:16 c, c, c, ] [ a a a a ]
		[ c, f, b, e ] 
		[ c,16:32 c, c, c, ] [ a16:32 a a a ]
%		% there's still some hairy beam bugfixing todo
		[ c'8:16 g d a, ]
		[ c,8:32 f, b, e ]
%		[ c'16:32 g d a, ]
		[:32 c16 e]
		[:32 c16 e]
		[:16 c8 e]
		[:16 e4 g]
		[:16 e2 g]
		[:16 e1 g]

	}
	\paper{
		castingalgorithm = \Wordwrap;
	}
}
