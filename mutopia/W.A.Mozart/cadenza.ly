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
%}

\version "1.0.21";


cad = \notes  \relative c' {
	\property Score.instrument = "french horn"
	\context Staff {
	\cadenza 1;
	\grouping 1*4;

	\clef "violin";
	c'4.\mf g8


	[e'^"accel" () d  c b]
	[b() c] g-\fermata
		\bar "empty";
			c, ~ [c^\turn_"rubato" e g c]
	e4. e8 [g () f_"rit" e d]

	dis4() e4
		\bar "" ;
		r8 [c16 d] [e f g gis]

	a4-> f() e g
	f-> d() cis e

	d4^\fermata
		\bar "" ;

	r8 a [b cis]
	\grace { e8( }
	[d16 cis d e]
	f4 ~ [f16 e d c]
	b4-\turn
	\times 2/3 { [ d8 c8 a8] }
	g2~
		\bar "" ;
	[g16 c, e g] [c e, g c]
	[e g, c e] g4^\fermata 
		\bar "" ;
	[g8.(_"a tempo" e16 g8. )e16]
	a4. g8 [f8 e8 d8 c8]
	g2 d'2-\trill
	\grace { [c32 d] }
	 c4
	} }
	
\score {
	\notes { \cad }
%	\midi { \tempo 4 = 90; }
	\paper {  casting_algorithm = \Wordwrap;}

}
