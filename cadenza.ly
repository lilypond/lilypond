%
% Cadenza to Mozart Horn/3
%

cad = music { $
	\duration { 8}
	'c4. g8 ['e()'d 'c b]
	[b()c] g c [c e g 'c]
	\octave{'}
	e4. e [g ()f e d]

	\duration{16}
	dis4()e4 r8 [c d] [e f g gis]
	\duration{4}
	a `f()`e g
	f `d()`cis e
	\duration{8}	
	d4 r8 `a [`b cis]
	\duration{16}
	[d cis d e]
	f4()[f e d c] `b4
	\octave{} ['c8 b8] % triplet!
	g2 [g c e g] ['c e g 'c]\octave{'} 
	[e `g c e] g4 %()% BUG!
	[g8.( e g8. )e]
	a4. g8 [f8 e8 d8 c8]
	`g2 d2
	c4 $}

score {
	staff { melodic
		music { cad }
	}
	commands { meter 4 4 skip 13:0 }
}