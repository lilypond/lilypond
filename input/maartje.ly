% The purpose of this file is to demonstrate features of LilyPond; 
%
% COPYRIGHT: GPL
%
%

ritme = staff {
	rhythmic
	music {$
	c8
	|[a8() a8. a8 a16 a16 a16] c4.
		
	%[c8( )'a8() c8 c8]% BUG!
	|c2 c2
	
	|[fis16 'dis16( fis16 'dis16 ][fis16) 'dis16 fis16 'dis16]
	| r32 r32 r16 r8 r4 r2
	|[c8. c16] [c16 c8.] [c16 c16 c8] [c16 c8 c16]

	 c2 c2 c2 c2 c2 c2 c2 c2 c2 c2

	$}
}

melody=
staff {
	melodic
	music{$
	c8\key{fis cis gis}
	|r4 r4 r4 r4
	|'cis2..	r8
	| r4 r8 r16 r32 r32 

	\duration {4}
	{
		\music { c () `bes [c8 c8] }
		\music { 'fis ()'gisis fis8 fis8 }
		\music { d () d dis8 dis8 }
		\music {  a  () bes eis8 eis8 }
		\music { fis () g gis8 gis8 }
	}
	{ c4 'c4 }

	[d8 e8 'f8 g8]  d8 e8 f8 g8
	|''fis2
	| a8 b8 'c8 'd8 |''c8 ```c8 c4 |c4  c4 |c4
	\duration{ 16 } `b `a `g `f \duration{ 4}
	\clef\bass	

	|c `b `a `g `f `e `d `c ``b ``a ``g ``f ``e ``d ``c
	$}

}

score {
	paper {
		geometric 1.4
		unitspace 3.0 cm
	}
%	staff { ritme } % broken for now
	staff { melody }
	commands {
		meter {4* 4}
		partial {8}
		skip {1*8}
		skip {3*2 }
		bar ":|:"
		skip {1*2}
		meter {2*4}

	}
}



