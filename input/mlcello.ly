% mlcello.ly
% 
% Viola Quartet
% Martien Lohman (194x-????) 
%
% Cello
% 
% quite alive at the time of writing; 
% copyright by ml
%
% The purpose of this file is to demonstrate features of LilyPond;
% respect the copyright.
%
cello = \melodic  {
	\clef"bass";
	\octave 'c;
	\duration 4;
	\key fis;
%%1
	g 'g |
%%2
	[ 'g8. 'a16 ] ''b |
%%3
	c d |
%%4
	g( [ ) g16 fis16 e16 fis16 ] |
%%5=1
	e2 |
%%6=2
	[ 'g8. 'a16 ] ''b |
%%7
	c d |
%%8
	'g d |
%%9=1
	'g2 |
%%10
	'g ''b |
%%11=4
	e [ g16 fis16 e16 d16( ] |
%%12
	) d g |
%%13
	[ d8 d'8 ] g |
%%14=10
	[ d8 d'8 ] g |
%%15
	[ c8 e8 ] [ g8 'g8 ] |
%%16
	\textstyle "italic";
	[ d8 c'8 ] b_"fine"
	\textstyle "roman" ;|
% \newline
%%17=1
	g 'g |
%%18=2
	[ 'g8. 'a16 ] ''b |
%%19=3
	c d |
%%20=4
	g( [ ) g16 fis16 e16 fis16 ] |
%%21=5=1
	e2 |
%%22=6=2
	[ 'g8. 'a16 ] ''b |
%%23=4
	c g |
%%24
	a 'a |
%%25
	d2 |
%%first modulation
%%26
	d 'fis |
%%27
	''b [ dis16 c16 ''b16 'a16( ] |
%%28
	) 'a d |
%%29
 	\duration 8;
	[ 'a a ] d4 |
%%30
	[ 'a a ] d4 |
%%31
	[ 'g ''b ] [ d 'd ] |
%%32
	[ 'a a ] d4 |
%%variant a
%%33
	\duration 16;
	[ 'g8 'g 'g ] [ 'g8 'g 'g ] |
%%34
	[ 'g8 'g 'g ] g4 |
%%35
	[ c8 c c ] [ c8 c' c' ] |
%%36
	g4 b4 |
%%37
	[ a8 a a ] [ 'a8 'a 'a ] |
%%38
	[ ''b8 ''b ''b ] [ ''b8 ''b ''b ] |
%%39
	[ 'a8 'a 'a ] [ 'a8 'a 'a ] |
%%40
	'd4 'g4 |
%%41
	\duration 8;
	\textstyle "italic";
	r_"pizz"
	\textstyle "roman";
	g r 'g |
%%42
	r g e4 |
%%43
	r b r ''b |
%%44
	r e a4 |
%%45
	r e r e |
%%46
	r ''b [ d g ] |
%%47
	r e r 'a |
%%48
	r ''b
	[ 'd 'g ] |
%%theme 2
%%49
	\textstyle "italic";
	r4_"arco"
	 [ 'g 'g 'g ]2/3  |
	\textstyle "roman";
%%50
	r4  [ c c c ]2/3  |
%%51
	g2 |
%%52
	c2 |
%%53
	r4  [ 'g 'g 'g ]2/3  |
%%54
	r4  [ c c c ]2/3  |
%%55
	g2 |
%%56
	'g4 'c4 \key bes es as;|
% \key\'es ||
%%57
	g r g r |
%%58
	c r c r |
%%59
	'g r 'g r |
%%60
	c r c r |
%%61
	d4. ''b |
%%62
	c2 |
%%63
	g4 'g4 |
%%64
	c4 'c4 |
% \meter 6/8; ||
%%65
	[ c c c ] c4.  |
%%66
	[ 'c 'c 'c ] 'c4.  |
%%67
	[ 'g 'g 'g ] 'g4. |
% \newline	
%%68
	[ g g g ] g4.  |
%%69
	c2.  |
%%70
	c2.  |
%%71
	g2.  |
%%72
	c' r4 r4. |
%%73
% \meter 2/4; ||
	g r g r |
%%74
	c r c r |
%%75
	'g r 'g r |
%%76
	c r c4 |
%%77
	d4. ''b |
%%78
	c2 |
%%79
	g4 'g4 |
%%80
	c4 'c4 |
%%81
	\duration 2;
	'c |
%%82
	'c |
%%83
	'e |
%%84
	'f |
%%85
	'as |
%%86
	c |
%%87
	d |
%%88
	\textstyle "italic";
	d_"dacapo" \key fis; |
	\textstyle "roman";
% \key\'g ||
	
}
