% "Ah, je vous dirai, maman" (Mozart)
% 
% bare bones version. (written down from memory :-)

melodie = $ \octave {c}
	%%% theme
	  c c g g	a a	g2	f f	e e	d d  c2
	  g g	f f	e e 	d d	g g	f f	e e 	d d
 	%%% var 1
	  c r8 c8 (	) g r8 g8 (	) a r8 a8 (	) g r4 
	  f r8 f8 (	) e4 r8 e8 (	) d4 r8 d8 (	) c4 r4
$

begeleiding = $
	\octave {`c}
	%%% theme
	`c c	e c	f c	e c	d `b	c `a	`f `g	`c2
	\octave {`c}
	e `g	d `g	c `g	`b `g	e `g	d `g	c `g	`b `g
	%%%% var 1
	r8 e8() c  	r8 e8() c  	r8 f8()c 	r8 e8()c
	r8 d8()`b 	r8 c8()`a 	r8 `a8()`f 	r8 `e8()`c
$


bstaf = melodicstaff {
		voices { begeleiding }
		commands {
			clef bass
		}
	}
vstaf = melodicstaff {
		voices { melodie }
	}


score {
	staff { vstaf }
	staff { bstaf }
	paper {
%		unitspace 2.5cm
	}
	commands {
		meter 2 4 
		skip 32:0
	}
}

