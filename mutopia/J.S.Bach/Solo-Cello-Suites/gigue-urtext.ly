% gigue-urtext.ly
% belongs together with -cello.ly and -viola.ly
% (who is going to make a -violin.ly?)

%{
Well, there are still some scripts in this "urtext".
But merging melodic and scripts doen't work too well yet (see viola_scripts).
%}

%
% this must be redone a bit:
% 
% * slurs/ties in two different threads (like beams): urtext, and additionals
% 

gigue_notes = \context Staff \notes \relative c {
	\context Voice=i
	a'8 |
	d,4 bes'8 | cis,4 g'8 | f16 e f g a8 |
	d,4 d'8 | e,16(f)g8 bes | c,16(d)e8 c' | a16 g a bes c a |
	%8
	f4-\trill a8 | b,()g' cis, | d f16( c bes )a | g8()es' a, |
	bes d16( a g )f | e!8()cis' bes' a g16( f e)d |
	%15
	\stemup e(f g e f)d \stemboth e8()a, c'! |
	\stemup c16(d es c d)a bes8 r bes |
	bes16(c d bes c)g a8 r a |
	%21
	b16()c d()b c()a | d()e f()d e()c | b()c d()b c()a | e'4 gis,8 |
	a16()b c()a d()b |
	a()b c()a d()b |
	%27
	a()b c()a d()b |
	a()b c()a d()b |
	f'8 e16()d c()b \stemboth | 
	e,, e''(d)c b()gis | a f(e)d e()cis | a4
	%33
	c'!8 |
	f,4 d'8 | e,4 bes'8 | a16 g a bes c8 | f,4 a8 |
	d,16(e)f d(c)b | g' a g f e d | 
	%39
	e(d)e f(g)e | c4 e8 | fis16(g)a c, bes a | 
	bes(d)g bes, a g | fis(a)c es d c | bes(a)bes d g bes |
	%45
	as(g)as fis g es' | d,8 g fis | g16 es(d)c d()bes | g4 bes'8
	% what about this?
	\stemup\stemdown e,!16(f)g e f d\stemboth
	c(d e) c d bes | a(bes)c a bes g | f4 a'8
	% what about this?
	\stemup\stemdown d,16(e)f d e c\stemboth bes(c)d bes c a 
	g16(a)bes g a f | e4 g'8 
	%57
	a,16(b cis d)e g-. | f(g a cis )d f, | e(f g a)bes d,-. |
	cis(d)e a, bes g 
	\stemup
	d'()e f()d g()e |
	%62
	d()e f()d g()e |
	d()e f()d g()e |
	d()e f()d g()e |
	\stemboth
	cis bes'(a g f)e | f, a' g f e cis |
	%67
	d bes a g a f | d a' d e f d | 
	\stemup es()f g()es f()d | g()a bes()g a()f es()f g()es f()d |
	%72
	cis'4 \stemboth d16()c | bes(a g f e)d | cis(b a g f)e | d f a d f a |
	d4
}

gigue_b = \context Staff \notes \relative c {
	\context Voice=ii
	\stemdown
	s8
	s4.*14
	a4. | s d | d8 g, d' | e4. | f8 f, f' |
	%21
	d d d  d d d  d d d |
	% one could type r here...
	d4 s8 | <e c> s s | d s s | e s s | f s s | gis s4 |
	s4. s s4 
	%33
	s8
	s4.*16
	\stemup bes4 s8
	s4. s s 
	a8 s4 \stemdown
	s4.*7
	<a,8 f> s s g s s a s s bes s s
	s4.*4
	g8 g g  g g g  g g g 
	%72
	<g4 e'>
}

gigue_a = \context Voice \notes<
	\$gigue_notes
>

gigue = \notes<
	\$gigue_a
	\$gigue_b
>

\version "1.1.55";
