\header{
filename =	 "fugue1.ly";
title =	 	 "Fuga a 4";
description =	 "Das Wohltemperierte Clavier I, Fuga I (c-major)";
opus =           "BWV 846";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "Shay Rojansky";
copyright =	 "Public Domain";
}

%{
	
%}
\version "1.0.7";


global = 
	\notes {
	\time 4/4;                % should be \time C
	}
	
tenor =
	\notes \relative c' {
	\clef "violin";

	\stemdown
	r8 [c8 d e] [f. g32 f] [e8 a] |
	[d, g] ~ [g16 a g f] [e f e d] [c d c b ] |
	[a 8 fis'] g4 ~ [g8 fis16 e] [f8 d] |
	[g f! e d] c r r g' ~ |
%% 5
	[g f16 e] f4 ~ [f16 f e8] d4 |
	[c8 f] r16 [g f e] [f8 d] g4 ~ |
	g4 r4 r2 |
	r1 |
	r8 [g8 a b] [c . d 32 c ] [b8 e ]
%% 10
	[a, d ] ~ [d 16 e  d  c ] b8 r8 r d, |
	[e fis] [g. a32 g] [fis8 b e, a] ~ |
	[a16 b a gis] [fis8 f!] [e d] ~ [d16 e fis gis] |
	[a gis a b] [gis fis gis a] b8 r8 r4 |
	r8 [c,8 d e] \stemup [f. g32 f] [e8 a] |
%% 15
	[d, g] ~ [g16 a g f] e8 \stemdown [e fis g] ~ |
	[g8 fis] [gis8 a] ~ [a g! a b] |
	\stemboth
	[c . d 32 c ] [b8 e ] [a, d ] ~ [d 16 e  d  c ] |
	[b8 g'  cis,  d ] [e  cis  d  e  ] |
	\stemdown a,8 r8 r e [fis g] \stemboth [a. b32 a] |
%% 20
	[g8 c  fis, b] ~ [b16 c  b a] \stemdown [g fis e d] \stemboth |
	e4 \stemdown d ~ [d16 a' g f!] [e g f! a] |
	g4 ~ [g16 a bes8] c 4 [d 8 g,] |
	\stemboth g4. f8 ~ [f e] d4 |
	\stemdown e8 a4 g8 ~ [g8 f g a] |
%% 25
	[bes8. c 32 b] [a8 d ] [g, c ] ~ [c 16 d  c  bes] |
	[a bes a g] [f g f e] d4 ~ [d8. g16] |
	a4 r16 [f' 16 d 8] e 2 |
	}


soprane =
	\notes \relative c' {

	\stemup
	r1 |
	r2 r8 [g'8 a b] |
	[c . d 32 c ] [b8 e ] [a, d ] ~ [d 16 e  d  c ] |
	[b g a b] [c  b c  d ] [e  d  e  fis ] [g 8 b,] |
%% 5
	[c  a] [d 16 c  b a] [g8. g16] [f e f g] |
	[a g a b] c 2 b4 |
	r8 [c8 d e] [f. g32 f] [e8 a] |
	[d, g] ~ [g16 a g f] [e8 a] ~ [a16 b a g] |
	f2 [e8. fis16] g4 ~
%% 10
	g4 fis [g16 f! e d] [c d c b ] |
	[a 16 c b  a ] g 8 r8 r16 [c16 b  a ] [gis 8 e'] |
	[d c16 b 16] [a  gis  a  b ] [c fis,  a  gis ] [b 8 a 16 b ]
	[c8 f e d] ~ [d c16 b ] [b 8. a 16] |
	a 4 r4 r2 |
%% 15
	r2 r8 [g 8 a  b ] |
	[c8. d32 c] [b 8 c] [d e] [f!. g32 f!] |
	[e8 a d, g] ~ [g16 a g f] [e8 a] |
	[d,8 bes'] [a g16 f] [g f g e] [f g g f32 g] |
	[a16 cis, d g] [e8. d16] d8 r8 r4 |
%% 20
	r2 r4 r8 g,  |
	[a  b ] [c. d32 c] [b 8 e a,  d] ~ |
	[e16 e d c] [b  c d e] [f g a g] [f e d c] |
	b 4 [c,8 d] g  c4 b 8 |
	c4 [b 8 bes 8] a  d4 c8 |
%% 25
	[d8 e] f4 ~ [f16 a g f] [e f e d] |
	c2 r16 [g 32 a  b !16 c] [d e f8] ~ |
	[f32 c d e f16 g] [a8. b16] <g2 c 2> |
	}

alt =
	\notes \relative c {
	\clef "bass";

	
	\stemup
	r1 |
	r |
	r |
	r8 [g'8 a b] [c . d 32 c ] [b8 e ] |
%% 5
	[a, d ] ~ [d 16 e  d  c ] [b8 c  ~ c  bes8] |
	[a d  g, c ] r16 [a16 b c ] d 4 |
	g,4 r8 g8 [a b] [c . d 32 c ] |
	[b8 e  a, d ] ~ [d 16 e  d  c !] [b!8 e ] ~ |
	e 4 d 4 [c 16 b c  a] [e'  d  c  b] |
%% 10
	[c 16 a b c ] [d  c  b a] g4 r4 |
	r1 |
	r8 [e8 fis gis] [a. b32 a] [gis8 c ] |
	[fis, b] ~ [b16 c  b a] gis8 a4 gis8 |
	a4 r8 g8 [a b] c . [d 32 c ] |
%% 15
	[b8 e  a, d ] ~ [d'  g,] d 4 |
	[c 8 a] e' 4 d 8 r8 r4 |
	r8 [a8 b cis ] [d . e 32 d ] [c !8 f ] |
	[b,8 e ] ~ [e 16 f  e  d ] cis 8 r8 r4 |
	r8 [a8 b cis ] [d . e 32 d ] [c !8 fis ] |
%% 20
	[b, e ] ~ [e 16 fis  e  d ] c 2 ~ |
	[c 16 d  c  b] [a g a fis] [g8 b c  d ] |
	[e . f 32 e ] [d 8 g ] [c,  f ] ~ [f 16 g  f  e ] |
	d 4 [e 8 d ] ~ [d  g,] g4 ~
	[g8 c, d e] [f. g32 f] [e8 a] |
%% 25
	[d, g] ~ [g16 a g f] [e d e f] [g a bes g] |
	[a e f g] [a b c  a] b2 |
	c 1 |
	}

bass =
	\notes \relative c {
	\clef "bass";


	\stemdown
	r1 |
	r |
	r |
	r |
%% 5
	r2 r8 [c8 d e] |
	[f. g32 f] [e8 a] [d, g] ~ [g16 a g f] |
	[e f e d] [c d c b ] [a' 8 d a fis] |
	[g16 a bes g] [cis,8 d] a'4 e4 |
	[a16 b c  d ] [c  b a g] c 8 r8 r4 |
%% 10
	r2 r8 [g, 8 a  b ] |
	[c. d32 c] [b 8 e] [a,  d] ~ [d16 e d c] |
	b 8 e4 d8 c f!4 e8 ~ |
	e d4 e8 [f! e16 d] e4 |
	a, 4 r4 r2 |
%% 15
	r8 [g 8 a  b ] [c. d32 c] [b 8 e] |
	[a, 8 d] ~ [d16 e d c] [b 8 bes  a  g'' ] |
	[a,, 8 fis' g e] d4 [e8 f!] |
	[g8. a32 g] [f8 bes] [e, a] ~ [a16 bes a g] |
	[f e f d] [g8 a] d,2 ~ |
%% 20
	[d16 e d c] [b  a  g  fis ] [e' 8 e fis g] ~ |
	[g a16 g16] [fis8 d] g2 ~ |
	g2 a4 [b8 c ] |
	[f,16 a g f] [e d c b ] [c d e f] [g8 g, ] |
	c1 ~ |
%% 25
	c1 ~ |
	c1 ~ |
	c1 |
	}

	
\score {
	\notes \type GrandStaff < 
		\type Staff = one <
			\global 
			\soprane
			\tenor
		>
		\type Staff = two <
			\global
			\alt
			\bass
		>
	>

	\paper{
	gourlay_maxmeasures =5.;
	castingalgorithme = \Wordwrap
	}
%	\midi { \tempo 4 = 84; }
}

% EOF
