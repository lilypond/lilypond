\header{
filename =	 "cadenza.ly";
title =	 "Cadenza ad libitum";
description =	 "Cadenza to Mozart Horn concerto 3";
composer =	 "unknown";
enteredby =	 "HWN";
copyright =	 "public domain";
}

%{
Tested Features: cadenza mode

Ugh.. Wish we had grace notes....  It adds another dimension to this
piece of music.  %}

\version "0.1.8";


cad = \melodic  {
	\property Score.instrument = "french horn"
	\type Staff {
	\cadenza 1;
	\grouping 1*4;
	\duration  8;
	\octave c';

	\clef "violin";
	c'4.\mf g8


	[e'^"accel" () d' c' b]
	[b() c'] g-\fermata
		\bar "empty";
			c [c_"rubato" e g c']
	\octave c'';
	e4. e [g () f_"rit" e d]

	\duration 16;
	dis4() e4
		\bar "" ;
		r8 [c d] [e f g gis]
	\duration 4;
	a-> 'f() 'e g
	f-> 'd() 'cis e
	\duration 8;	
	d4^\fermata
		\bar "" ;

	r8 'a ['b cis]
	\duration 16;	[d cis d e]
	f4() [f e d c]
	'b4-\turn
	\octave c'; [2/3 d'8 c'8 a8]1/1
	g2
		\bar "" ;
	[g c e g] [c' e g c']\octave c''; 
	[e 'g c e] g4^\fermata 
		\bar "" ;
	[g8.(_"a tempo" e g8. )e]
	a4. g8 [f8 e8 d8 c8]
	'g2 d2-\trill
	c4
	}}
\score {
	\melodic { \cad }
	\midi { \tempo 4 = 90; }
	\paper {
	}
}
