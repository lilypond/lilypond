#(set! point-and-click line-column-location)

piece = "5"
opus = "BWV 926"

\version "1.3.146"

upper =  \context Staff \notes\relative c
	\context Voice=i {
	\property Voice.TextScript \set #'font-style = #'italic


	[d'8 a' f d a' f] |
	d a' f d a' f |
	d bes' g d bes' g |
	d bes' g d bes' g |
	%5
	cis, g' e cis g' e |
	cis bes' g e a g |
	f d f a f a |
	d a d f d f |
	b  f e d c b |
	%10
	a gis fis e d' b |
	< 
		{
			\stemUp 
			\property Voice.TextScript \set #'font-style = #'roman
			c4-\mordent^"(\\textsharp)" r r |
			c4-\mordent^"(\\textsharp)" r r
			\property Voice.TextScript \set #'font-style = #'italic
		}
		\context Voice=ii { \stemDown a4 r r | a4 r r }
	> |
	\stemBoth 
	a'8 es d c bes a |
	g fis e! d c' a |
	%15
	bes8-\mordent d8 bes g g'4 |
	r8 d c bes a-\prall g |
	a c a f  f'4 |
	r8 c bes a g-\prall f |
	g bes a g f e |
	%20
	f d f a d g, |
	< 
		{ 
			\stemUp 
			% ... textnatural
%			cis8-\prall e8-"poco cresc." cis a e' cis 
			cis8-\prall e8 cis a e' cis 
		}
		\context Voice=ii { \stemDown a4 }
	>
	\stemBoth 
	a8 e' cis a bes! a |
	g e' cis g e' cis |
	g e' cis g a g |

	f d' bes f d' bes |
	%25
	f d' bes f d' bes |

	fis c' a fis c' a |
	fis c' a fis c' a |
	bes g fis g d g |
	bes g d bes' g d |
	%30
	e g fis g bes g |
	es bes' g es bes' g |
	cis, bes' g cis, bes' g |
	cis, bes' g cis, a' g |
	f a f d a' f |
	%35
	d a' f d cis d |
	e g e bes g' e |
	bes g' e cis a g' |
	f16 d c bes \stemUp a s16 s8 s4 |
% ugh
%	s1 |
	s4 s4 s4 |
	%40
	s4 s16 [d16 f a] \stemDown [d, f a] \stemUp d |
	\stemBoth
	f a f d  f d b d  gis, b a gis |

	% arpeggio
	<g'!4. e a,> a8 
	< f4  d a > ~ 
	[f8 e]
	< { \stemUp 
		e4.^\prall d8
		}
		\context Voice=ii { \stemDown cis2 }
	> 
	\stemBoth 
	d8 c! a d bes g |
	%45
	c a fis bes g e  |

	a fis d g e cis |

	%  the mordent is on the D !
	<fis2. d-\mordent a> 
}


lower =  \context Staff \notes\relative c{
	\context Voice=i

	d4-\mordent r r |
	d,4 r r |
	d'-\mordent r r |
	d,4 r r |
	d'-\mordent r r |

	d,4 r r |
	d'8 a d f d f |
	a f a d a d |
	gis,4 r r |
	e gis e |
	a8 e' c a e' c  |
	g! es' c g es' c  |
	fis,4 r r 
	d  fis d |
	\property Voice.TextScript \set #'font-style = #'roman
	g4-\mordent^"(\\textsharp)" r r8 f!8 |
	\property Voice.TextScript \set #'font-style = #'italic
	e4-\prall r r |
	f4-\mordent r r8 e |
	d4 r8 f e d |
	e d cis e d cis |
	d4 c! bes |
	a a' a, |
	a a' a, |
	a a' a, |
	a a' a, |
	bes r r |
	bes bes' bes, |
	a r r |
	a d d, |
	g r r |
	g g' g, |
	g r r |
	g g' g, |
	a r r |
	a a' a, |
	a r r |
	a a' a, |
	cis, r r |
	cis cis' cis, |
	d s16
	\stemDown
	[g'16 f e] f a d, f | 
	a, \stemUp [d c bes] 
	a \stemDown [ g f e] d f a d |
	\stemUp

	[f a]
	\stemDown
	[d, f] a s s8 s4 |
	\stemBoth
% ugh whole rest has duration of one bar
	R2. |
%	r4 r r |
	\stemBoth
	cis,8 e cis a  d16 c bes a |
	g8 g' a g a a, |
	% ah, both at last
	% what's going on here??
	\stemBoth
	\stemBoth
	\stemBoth
	d4 d' d, |
	d r r |
	d, d' d, |
	d2. |
}

global =  \notes{
	\time 3/4
	\key f \major
}

\score{
	% Moderato
	\context PianoStaff <
		\context Staff = upper <
			\global
			{ \upper \bar "|." }
		>
		\context Staff = lower <
			\global
			\clef "bass"
			\lower
		>
	>
	\paper{
		linewidth = 18.0 \cm  

	}
	\midi{ \tempo 4 = 90 }
	\header{
		piece = \piece
		opus = \opus
	}
}

