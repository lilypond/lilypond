% sarabande-urtext.ly
% belongs together with -cello.ly and -viola.ly
% (who is going to make a -violin.ly?)

%{
Well, there are still some scripts in this "urtext".
But merging melodic and scripts doen't work too well yet (see viola_scripts).
%}

sarabande_a = \context Staff \notes \relative c {
	\context Voice=i
	\stemup d8. e16 e4.-\trill d16 e |
	f4. \stemboth e8 d c |
	[bes g'] f e16(f g a bes)d, |
	cis4.-\trill b8 a g |
	% copy bar 1, half bar 2
	\stemup d'8. e16 f4.-\trill d16 e |
	%5
	f4. \stemboth d8 e f |
	g bes16()a c()bes a()g d'8 f, |
	\stemup e4.-\trill \stemboth d8 c bes |
	%8
	\stemup f' g16()a a4. g16()f |
	g8 a16()bes bes4. c16()d |
	% 11
	\stemboth e,8 f c, g' f' e |
	f4 f,2 |
	\stemup a'4 a4.-\trill bes8 |
	c bes16 a \stemboth fis8.-\trill es16 d8 c |
	bes g' a, fis' es' d |
	%16
	\stemup bes4.-\trill \stemboth a8 g f! |
	e bes a f' g a |
	d, as g es' f g |
	cis, bes' a g16 f e!8 f16 d |
	cis8 e16 a a,8. g'16 f8()e |
	%21
	\stemup d e16()f f4. e16()d |
	e8 f16()g g4. a16()bes |
	a8 cis16 d d,8 e16 f32 g f8-\trill e16()d |
	d4 d,16 a'( b cis d e f )g |
	%25
	a(b c)b c4. b16()a |
	b cis d cis d4. e16()f |
	\stemboth d(cis)d f,  a,8 e' d' cis |
	d4 d,,2 |
}

sarabande_b = \context Staff \notes \relative c {
	\context Voice=ii
	\stemdown
% dispute
%	d4 a2 |
%	this avoids the clash bug
	d8. s16 a2 |
	<a4. d,> s8*3 |
	s2.*2
	%5
%disp
%	<a4 f> a2 |
	<a8. f> s16 a2 |
	<a4. d,> s8*3 |
	s2. |
	% 8
	<g4. c,> s8*3 |
	a4 <bes4. d> r8 |
	%10
	bes4 <g2 f'> |
	s2.*2 |
	f'8 es es4. r8 |
	d4 s2 |
	%15
	s2. |
	<d4. g,> s8*3 |
	s2.*4 |
	%21
	bes4 g2 |
	g4 <bes4. cis,> s8 |
	<d8 a f> r r g, a4 |
	s2. |
	f'4 fis4. s8 |
	<d4 g,> gis4. s8 |
	s2.*2
}


sarabande = \context Staff \notes<
	\$sarabande_a
	\$sarabande_b
>

\version "1.3.59";
