%
% Cadenza to Mozart Horn/3
%
% author: unknown. Copyright: none
%

cad = music { $
	\duration { 8}
	\textstyle "italic"
	\command { \clef \violin }
	'c4._"f" g8
 	\textstyle  "roman" 

	['e^"accel" ()'d 'c b]
	[b()'c] g-\fermata
		\command { \bar \empty }
			c [c_"rubato" e g 'c]
	\octave{'}
	e4. e [g ()f_"rit" e d]

	\duration{16}
	dis4()e4
		\command { \bar \empty }
		r8 [c d] [e f g gis]
	\duration{4}
	a-> `f()`e g
	f-> `d()`cis e
	\duration{8}	
	d4^\fermata
		\command { \bar \empty }

	r8 `a [`b cis]
	\duration{16}	[d cis d e]
	f4()[f e d c]
	`b4
	\octave{} \plet{2/3} ['d8 'c8 a8]
	\plet{1/1}
	g2
		\command { \bar \empty }
	[g c e g] ['c e g 'c]\octave{'} 
	[e `g c e] g4^\fermata %()% BUG!
		\command { \bar \empty }
	[g8.(_"a tempo" e g8. )e]
	a4. g8 [f8 e8 d8 c8]
	`g2 d2^"tr"
	c4
	$}

	score {
	staff { melodic
		music { cad }
		
	}
	commands {
		meter 4*4
		cadenza 1
		grouping 1*4
	}
}
