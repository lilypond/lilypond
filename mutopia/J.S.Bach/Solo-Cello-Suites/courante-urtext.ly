% courante-urtext.ly
% belongs together with -cello.ly and -viola.ly
% (who is going to make a -violin.ly?)

%{
Well, there are still some scripts in this "urtext".
But merging melodic and scripts doen't work too well yet (see viola_scripts).
%}

couranteA =  \context Staff \notes \relative c {
	\context Voice=i
	d'16 |
	d a f a d, f g a bes a bes g |
	\stemUp <g4 a> s4 \stemBoth g16 f g e |
	% urg, a good case for mean-distance-beam-dir-algorithm!
	f d e c! bes(a)bes a' g f e d |
	%4
	cis e a,(b cis d e f )g bes-- a e |
	f a d,( e f g a bes )c bes-- d c |
	\stemUp <c4 f,> ~ < c16 f,> bes a g \stemBoth f()es d()es |
	%7
	d bes(a)bes d bes e! bes f' bes, g' bes, |
	e, g c d e f g a bes a bes g |
	a f(e)f a f bes f c' f, d' f, |
	%10
	cis( e )a b cis d e f g()f g()e |
	d,( a' )f' e d()c d()c b()a b()a |
	gis( a )b e, f()e f()d e()c d()b |
	%13
	c a'( b c b a gis)a d, a' e a |
	f a( b c b a gis)a f a e a |
	dis, a'( b c b a gis)a e a e gis |
	a e cis e a, cis e gis [ a8. ]
	e'16
	%17
	e cis a cis e, f g a bes()g cis()g |
	\stemUp d'4 ~ d16 a d e f d a c! \stemBoth |
	b16 f(d)f g, d' f g b f d' f, |
	%20
	e g c, d e f g a bes! d c g |
	\stemUp a s16*3 s4 s4
	d, ~ d16 e! f g \stemBoth a bes c d |
	%23
	e, bes a g a f' g,()f c g' f' e |
	f a bes a g f e f g e f d |
	cis g' b, g' a, g' b, g' cis, g' a, g' |
	%26
	f d f a d a d e f a, f d |
	g, d' g a bes g' a, f' g, e' f, d' |
	cis(d)e cis a cis bes cis a cis g cis |
	%29
	f, d'(e f e d cis)d g, d' a d |
	bes d e f e d cis d bes d a d |
	gis, d'(e f e d cis)d a d a cis |
	d a f a d, f a, d 
	d,8.
	\bar "|.";		
}

couranteB =  \notes \relative c {
	\context Voice=ii
	\stemDown
	s16 |
	s2. |
	%2
	cis4 ~ cis16 d e f s4 |
	s2.*3 |
	%6
	a,4 s2
	s2.*9
	% 16
	s2 s8.
	s16
	s2.
	%18
	f'4 s s |
	s2.*2
	%21
	\slurUp
	f16 c'(bes a g f es d )es c' f, es |
	\slurBoth
	bes4 s s
	s2.*9
	s2 s8.
}

courante =  \context Staff \notes<
 \couranteA
 \couranteB
>

\version "1.3.117";
