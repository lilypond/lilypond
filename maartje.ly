ritme = staff {rhythmic
	music {$
	c8 c2 c2
	%[c8( )'a8 c8 c8]
	c2 c2
	
	[fis16 'dis16( fis16 'dis16 ][fis16) 'dis16 fis16 'dis16]
	c2 r32 r32 r16 r8 r4
	[c8. c16]
	[c16 c8.]
	[c16 c16 c8]
	[c16 c8 c16]

	c2 c2 c2 c2 c2 c2 c2 c2 c2 c2 c2 c2


	$}
}

melody=
staff { melodic
	music{$
	c8
	r1
	'cis2..	r8
	r4 r8 r16 r32 r32 

	\duration {4}  \chord {
		\music { c () `bes c}
		\music { 'fis ()'gisis fis }
		\music { d () d dis }
		\music {  a  () bes eis }
		\music { fis () g gis }
	}
	c4

	[d8 e8 'f8 g8]  d8 e8 f8 g8
	''fis2
	a8 b8 'c8 'd8 ''c8 ```c8 c4 c4  c4 c4
	\duration{ 16 } `b `a `g `f \duration{ 4}
	

	c `b `a `g `f `e `d `c ``b ``a ``g ``f ``e ``d ``c
	$}

	commands {
	skip 0:0.125
	skip 1:0 % BUG!!
	key $ fis cis gis $
	skip 2:0
	key $ $
	skip 5:0
	clef bass
}
}

score {
	paper {
		geometric 1.4
		unitspace 3.0 cm
%		symboltables { table_sixteen}
	}
	staff { ritme }
	staff { melody }
%	staff { melody }
	commands {
		meter 4 4
		partial 0.125 skip 0:0.125
		skip 2:0		
		meter 2 4
		skip 19:0
	}
}



