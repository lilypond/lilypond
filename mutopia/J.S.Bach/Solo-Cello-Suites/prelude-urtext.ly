% prelude-urtext.ly
% belongs together with -cello.ly and -viola.ly
% who is going to make a -violin.ly? 

%{
Well, there are still some scripts in this "urtext".
But merging melodic and scripts doen't work too well yet see viola_scripts .
%}

% urg, Thread is gone, try Voice
% prelude_notes = \type Thread = prelude \notes \relative c {
prelude_notes = \type Voice = prelude \notes \relative c {
	d8 f a4 ~ a16 f e d |
	cis e g a bes4 ~ bes16 a g f |
	e g bes cis e8. bes16 a16 g f e |
	f g a f d8 c! bes a |
	%5
	bes16 d f a d8. c16 bes a g f |
	e g bes d c a bes g f e g bes, |
	a c e g c8. bes16 a g f e |
	d f a c bes g a f e f a f |
	%9
	g, bes d f bes8. a16 g f e g |
	a, c e g c a e g f a d, es |
	d a bes d g bes a c bes g d' f, |
	%12
	e b c e g d e c bes g e' bes |
	f8 a c4 ~ c16 bes a g |
	fis a bes c d, c' bes a fis' es d c |
	bes a g bes d4 ~ d16 c bes a |
	%16
	gis b c d e, d' c b gis' f e d |
	c b a c f e f gis a f d c |
	b d gis b d8. c16 b a gis a |
	%19
	c, e a c e8. c16 b a gis a |
	d, f a d f8. e16 d c b d |
	e, d' c b a c b a d, b' a gis |
	%22
	c, a' g! f cis g' f e d f e d |
	gis, d' e f b f e d gis, d' c b |
	a b c e a b c a e c a g! |
	% 25
	% B"arenreiter and Chester say "c bes c"
	fis a c d es8. d16 c bes c a' |
	bes, a bes d g, es' f g a, g' f es |
	d c d f bes, g' a bes cis, bes' a g |
	% 28
	f e f a d, bes' c! d e,! d' c bes |
	a g a c f, d' e f g, f' e d |
	cis g f e a, e' f g cis bes! a g |
	%31
	f g a cis d a g f a f e d |
	gis d e f a, f' e d gis f! e d |
	cis b cis e a e cis e a, g'! f e |
	%34
	f e f a d a f a d, c'! bes a |
	g f g cis e cis g cis a, g' f e |
	d a' d e f d a f d c'! bes a |
	%37
	g a bes d, es f g a bes g es' g, |
	f g a cis, d e! f g a f d' f, |
	e f g bes, a b cis d e bes g' bes, |
	%40
	cis,8 a' g'4 ~ g16 bes a g |
	f e d e f d a' f d' a f d |
	gis,8 f' d'4 ~ d16 f e d | 
	cis b a b cis a d a e' a, f' a, |
	%44
	g' e cis e a, cis e f g f g e |
	f d cis d a cis d e f e f d |
	e cis b cis a b cis d e d e cis |
	%47
	d b a b f gis b cis d cis d b |
	\voiceone <cis4^\fermata e,> \onevoice r r |
	bes16 g fis g es g d g es g bes d, |
	cis-- e! g a bes8. a16 g fis g e' |
	%51
	f,! d' bes g a f e g f d cis e |
	d bes a g fis-- a c! es d c bes a |
	bes g fis g es g d g es g bes d, |
	%54
	\voiceone g'8. f16 
	e!16 d cis b a g f e \onevoice |
	d-- a' d e f e d c! bes! a g f |
	e-- a cis e g f e d cis b a g |
	f a d f a d, f a d bes! c! a |
	g, d' g a bes g fis g es' g, d' g, |
	\voiceone <cis2. g> | <d f,> | <d e,> | <cis e,> | <d f,>
}

prelude_b = \notes \relative c {
	\voicetwo 
	s2.*47
	%48
	g4 s2 |
	s2.*5
	%54
	<cis,4 bes'> s2 |
	s2.*4
	a'2. a a a <a d,>
}

% pat1 = \notes { [s8 s]s4[s16 s s s] }
pat1 = \notes { [s16 s s s]s4[s16 s s s] }
pat2 = \notes { [s16 s s s][s8.s16][s s s s] }
pat3 = \notes { [s16 s s s][s8 s s s] }

% of course, i just type pat1, and add the \ and the silly $ later
% urg, Thread is gone, try Voice
% prelude_beams = \type Thread = prelude \notes{
prelude_beams = \type Voice = prelude \notes{
	\$pat1
	\$pat1
	\$pat2
	\$pat3
	%5
	\$pat2
	\$pat2
	\$pat2
	\$pat2
	\$pat2
	%10
	\$pat2
	\$pat2
	\$pat2
	\$pat1
	\$pat2
	%15
	\$pat1
	\$pat2
	\$pat2
	\$pat2
	\$pat2
	%20
	\$pat2
	\$pat2
	\$pat2
	\$pat2
	\$pat2
	%25
	\$pat2
	\$pat2
	\$pat2
	\$pat2
	\$pat2
	%30
	\$pat2
	\$pat2
	\$pat2
	\$pat2
	\$pat2
	%35
	\$pat2
	\$pat2
	\$pat2
	\$pat2
	\$pat2
	%40
	\$pat1
	\$pat2
	\$pat1
	\$pat2
	\$pat2
	%45
	\$pat2
	\$pat2
	\$pat2
	s2.
	\$pat2
	%50
	\$pat2
	\$pat2
	\$pat2
	\$pat2
	\$pat2
	%55
	\$pat2
	\$pat2
	\$pat2
	\$pat2
}

%{
properties, urg!
fig1 = \notes{ s16( s s )s }
fig2 = \notes{ s16( s ) s s }
fig3 = \notes{ s16 s( s )s }
fig4 = \notes{ s16( s s s s s s )s }
fig5 = \notes{ s8.()s16 }
%}

fig1 = \notes{ \slurnormal s16( s s )s }
fig2 = \notes{ \slurnormal s16( s ) s s }
fig3 = \notes{ s16 \slurnormal s( s )s }
fig4 = \notes{ \slurnormal s16( s s s s s s )s }
fig5 = \notes{ \slurnormal s8.()s16 }

% of course, i just type fig1, and add the \ and the silly $ later
% urg, Thread is gone, try Voice
% prelude_slurs = \type Thread = prelude \notes{
prelude_slurs = \type Voice = prelude \notes{
	s4 s4 \$fig3 |
	\$fig1 s4 \$fig3 |
	\$fig1 s4 \$fig3 |
	\$fig1 s8( s s )s  |
	%5
	\$fig3 s2 |
	s2. |
	s2 \$fig1 |
	s2 \$fig1 |
	s2 \$fig1 |
	%10
	\$fig3 \$fig3 \$fig1 |
	\$fig3 \$fig3 s4 |
	\$fig3 s2 |
	s4 s4 \$fig3 |
	\$fig3 \$fig3 \$fig3 |
	%15
	s4 s s16 s s8 |
	\$fig3 \$fig3 s4 |
	s2. |
	\$fig1 s4 \$fig1 |
	s2 \$fig1 |
	%20
	\$fig1 s4 \$fig1 |
	s4 \$fig3 \$fig3 |
	\$fig3 \$fig3 \$fig3 |
	s2 \$fig1 |
	s2. |
	%25
	\$fig3 s4 \$fig1 |
	\$fig2 \$fig3 \$fig3 |
	\$fig2 \$fig3 \$fig3 |
	\$fig2 \$fig3 s4  |
	\$fig2 \$fig3 \$fig3 |
	%30
	\$fig3 \$fig3 \$fig1 |
	\$fig2 \$fig3 \$fig3 |
	s2. |
	s4 \$fig3 \$fig3 |
	\$fig2 \$fig3 \$fig3 |
	%35
	\$fig2 s2 |
	s2. |
	s2. |
	s2. |
	s2. |
	%40
	s4 s4 \$fig3 |
	\$fig1 s2 |
	s2 \$fig3 |
	\$fig2 s2 |
	s2. |
	%45
	s2. |
	s4 \$fig2 s4 |
	\$fig3 \$fig2 s4 |
	s2. |
	\$fig3 s2 |
	%50
	\$fig3 s4 \$fig2 |
	s2. |
	s4 \$fig3 s4 |
	s2 \$fig2 |
%	s8()s \$fig4 |
%	s8.()s16 \$fig4 |
	\$fig5 \$fig4 |
	%55
	\$fig3 \$fig4 |
	\$fig3 \$fig4 |
	\$fig3 \$fig3 s4 |
	s4 \$fig3 s4 |
}

fig1 = \notes{ \slurdotted s16( s s )s }
fig2 = \notes{ \slurdotted s16( s ) s s }
fig3 = \notes{ s16 \slurdotted s( s )s }
fig4 = \notes{ \slurdotted s16( s s s s s s )s }
fig5 = \notes{ \slurdotted s8.()s16 }
fig6 = \notes{ \slurdotted s16()s \slurdotted s()s }

% urg, Thread is gone, try Voice
% prelude_suggested_slurs = \type Thread = prelude \notes{
prelude_suggested_slurs = \type Voice = prelude \notes{
	\slurdotted
	s2. |
	s2. |
	s4 \$fig5 s4 |
	s2. |
	%5
	s4 \$fig5 \$fig1 |
	\$fig1 \$fig1 \$fig2 |
	s4 \$fig5 s4 |
	\$fig1 \$fig1 s4 |
	\$fig2 \$fig5 s4 |
	%10
	s2. |
	s2 \$fig2 |
	s4 \$fig3 \$fig2 |
	s2. |
	s2. |
	%15
	\$fig2 s4 \$fig3 |
	s2 \$fig3 |
	\$fig1 \$fig1 \$fig1 |
	s4 \$fig5 s4 |
	\$fig1 \$fig5 s4 |
	%20
	s4 \$fig5 s4 |
	\$fig3 s2 |
	s2. |
	\$fig3 \$fig3 s4 |
	\$fig1 \$fig1 \$fig1 |
	%25
	s4 \$fig5 s4 |
	s2. |
	s2. |
	s2 \$fig3 |
	s2. |
	%30
	s2. |
	s2. |
	\$fig3 \$fig3 \$fig2 |
	\$fig2 s2 |
	s2. |
	%35
	s4 \$fig3 \$fig3 |
	s2 \$fig3 |
	s2. |
	\$fig2 s2 |
	\$fig2 s2 |
	%40
	s2. |
	s2. |
	s2. |
	s2. |
	% check !
	\$fig3 \$fig2 s4 |
	%45
	\$fig3 \$fig2 s4 |
	% check!
	\$fig3 s2 |
	s2. |
	s2. |
	s2 \$fig2 |
	%50
	\$fig1 \$fig5 s4 |
	s4 \$fig6 \$fig6 |
	\$fig3 \$fig1 \$fig1 |
	\$fig3 s2 |
}

prelude_a = \type Voice \notes<
	\$prelude_notes
	\$prelude_beams
	\$prelude_slurs
	\$prelude_suggested_slurs
>

prelude = \notes<
	\$prelude_a
	\$prelude_b
>

