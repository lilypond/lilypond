ritme = rhythmstaff {
	voice { $ c2 r32 r32 r16 r8 r4 c2 c2 c2 c2 c2
		c2 c2 c2 c2 c2 c2 c2 c2 c2 c2 c2 c2 c2 c2 $ }
}

melody=
melodicstaff {
  voice { $
	''c2.. r8 r4 r8 r16 r32 r32
	[d8 e8 f8 g8] [d8 e8 f8 g8]
	''fis1
	a8 b8 'c8 'd8 ''c8 ```c8 c4 c4  c4 c4
	\duration 16  `b16 `a16 `g16 `f16 \duration 4
	c `b `a `g `f `e `d `c ``b ``a ``g ``f ``e ``d ``c$
	}
}
score {
	paper {
		geometric 1.4
		unitspace 3.0 cm
	}
	staff { ritme }
	staff { melody }
%	staff { melody }
	commands {
		meter 4 4
		skip 2:0
		meter 2 4
		skip 19:0
	}
}



