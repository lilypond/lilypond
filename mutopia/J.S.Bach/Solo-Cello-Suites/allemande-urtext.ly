% allemande-urtext.ly
% belongs together with -cello.ly and -viola.ly
% (who is going to make a -violin.ly?)

%{
Well, there are still some scripts in this "urtext".
But merging melodic and scripts doen't work too well yet (see viola_scripts).
%}

allemande_a = \context Staff \notes \relative c {
	\context Voice=i
	\stemup
	a'16 | 
	a8 bes16()a \stemboth
	g()f e()d d()cis d()e a,8 bes16()g |
	f( a )d f,  e8 cis' \stemup d8. e16 \stemboth f g a bes! |
	%3
	\stemup c8 d16()es \stemboth d()c bes()a c()bes a()g d'8. f,16 |
	e( g )bes d c()bes a()g bes()a g()f 
	< f8. a> <f16 a > |
	% urg fix
	b f( e )d e cis' d cis \stemup d8. e16 \stemboth
	f( e d)e |
	%6
	d( c b )c b( a gis)a gis8-\prall fis16()e e' c( a )g |
	\stemup e' a, f d  d f d b \stemboth
	gis( b e )gis b d c b |
	%8
	c( a f )e d( f e )d gis8.-\trill()a16 b d e, d |
	% urg
	c e a d \stemup b8.-\trill a16 a8 s4 s8 \stemboth |
	%10
	d,!16 gis( a b a gis fis )e \stemup e8 f16()e \stemboth
	d()c b()a |
	\stemup d8 e16()f \stemboth e d c b 
	\stemup d'16()b c()a \stemboth e8 gis |
	a,8. cis16 e g! f! e f a d gis, [ a8. ]
	%13
	e16 \stemup e8. f16 \stemboth
	g!()e f()a cis,( d )e bes a8.-\trill()g16 |
	f a'(f)d g b,()cis a' g(f e)d fis d()es c( |
	%15
	)bes g'(a, )g fis a d c' bes(fis)g bes d()a bes()g |
	es(d)es g c()a bes()g d c d g bes()fis g()es |
	%17
	c(bes)c bes' a(c)es g, \stemup fis8-\trill g16 a \stemboth
	d,8 es16 c |
	bes d g bes, d,8 fis' \stemup g8. a16 \stemboth bes d g, f |
	%19
	\stemup e8.-\trill f16 \stemboth g e c bes 
	a()f' g,()f e g'( a )bes |
	bes( a g )f
	a()e f()d bes d(f)a d()a bes()g |
	%21
	a,()g'cis()d e()g, a()e f()d bes()d gis, f' e d |
	d( cis b )a
	c(a)fis d' c a( b )d 
	f!(d )gis, d' |
	%23
	cis(e g!)bes e()a, bes()g f()cis d()gis, a8 cis! |
	% knee
	\stemup d,16 \stemboth d''(c!)a bes!(g)e cis' d a f d d,8.
}

allemande_b = \notes \relative c {
	\context Voice=ii
	\stemdown
	s16 |
	% <d'8 g' f'> s8 s2. |
	<f8 g, d> s8 s2. |
	s2 <a,8. d,> s16 s4 |
	<fis'8 a, d,> s8 s2. |
	s1 | 
	%5
	s2 d8. s16 s4 |
	s1 |
	f!16 s16*3 b, s16*3 s2 |
	s1 |
	\property Voice.slurVerticalDirection=1
	s4 e [dis8 ~ dis32 e( fis gis] [a b c d! c b c )a] |
	\property Voice.slurVerticalDirection=0
	%10
	s2 <a8 c,> s8 s4 |
	<b8 gis,> s8 s4 <e,8 a,> s8 s4 |
	s2 s4 s8.
	s16
	<cis'8 g,> s8 s2. |
	%15
	s1*3 |
	s2 c,8 s8 s4 |
	s2 g8. s16 s4 |
	bes8. 
%	}
}

allemande = \context Staff \notes<
	\$allemande_a
	\$allemande_b
>


\version "1.2.0";
