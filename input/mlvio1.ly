% mlvio1.ly
% 
% Viola Quartet
% Martien Lohman (194x-????) 
%
% Violin I
% 
% quite alive at the time of writing; 
% copyright by ml
% 
% The purpose of this file is to demonstrate features of LilyPond;
% respect the copyright.
%
violin1 = music { 
	$
	\octave{}
	\duration{4}
%1
	g [ b8. a16 ] |
%2
	g2 |
%3
	g [ a16 b16 'c16 a16 ] |
%4
	g2 |
%5=1
	g [ b8. a16 ] |
%6=2
	g2 |
%7
	e fis |
%8
	d fis |
%9=1
	g [ b8. a16 ] |
%10
	g f |
%11=4
	e d |
%12
	[ fis8 a8 ] [ g8 'd8 ] |
%13
	[ 'd8 a8 ] [ 'd8 b8 ] |
%14=10
	a b |
%15
	'c b |
%16
	\textstyle "italic"
	a g_"fine"
	\textstyle "roman" |
%\newline
%17=1
	g [ b8. a16 ] |
%18=2
	g2 |
%19=3
	g [ a16 b16 'c16 a16 ] |
%20=4
	g2 |
%21=5
	g [ b8. a16 ] |
%22=6
	g2 |
%23=4
	e [ d16 c16 `b16 c16 ] |
%24
	cis 'cis |
%25
	'd [ 'fis8. 'e16 ] |
%first modulation
%26
	'd 'c |
%27
	b a |
%28
	[ a8 b16 'cis16 ] 'd |
%29
	'cis 'd |
%30
	'cis a |
%31
	[ g8 'd8( ] ) 'd |
%32
	'cis 'c |
%variant 'a
%33
	\duration{16} [ b8 'd 'd ] [ 'd8 'd 'd ] |
%34
	[ 'g8 'd 'd ] b4 |
%35
	[ 'c8 'c 'c ] [ 'c8 'c 'c ] |
%36
	b2 |
%37
	[ 'c8 'e 'e ] [ 'e8 'd 'c ] |
%38
	[ b8 'd 'd ] [ 'd8 'd 'd ] |
%39
	[ 'e8 'c 'c ] [ 'c8 'd 'c ] |
%40
	'c4 { b4 'd4 } |
%41
	\textstyle "italic"
	\duration{8}
	r_"pizz" d r g
	\textstyle "roman" |
%42
	r d e4 |
%43
	r b r 'd |
%44
	r 'e 'c4 |
%45
	r b r b |
%46
	r b [ a b ] |
%47
	r b r 'c |
%48
	r 'd [ 'd b ] |
%theme 2
%49
	\textstyle "italic"
	r4_"arco" \plet{ 2/3 } [ b8  b  b ] \plet{ 1/1 }
	\textstyle "roman" |
%50
	r4 \plet{ 2/3 } [ g g g ] \plet{ 1/1 } |
%51
	b2 |
%52
	'c2 |
%53
	r4 \plet{ 2/3 } [ b b b ] \plet{ 1/1 } |
%54
	r4 \plet{ 2/3 } [ g g g ] \plet{ 1/1 } |
%55
	b2 |
%56
	g2 |
% \key\Es ||
%57
	f r f r |
%58
	es r 'c r |
%59
	b r b r |
%60
	'c r 'c4 |
%61
	\duration{16}
	\plet{ 4/6 } [ as f d f as 'd ] \plet{ 1/1 } \plet{ 4/6 } [ as f d `as d f ] \plet{ 1/1 } |
%62
	\plet{ 4/6 } [ g es c `g c es ] \plet{ 1/1 } \plet{ 4/6 } [ g 'c 'es 'c g es ] \plet{ 1/1 } |
%63
	\plet{ 4/6 } [ g d g bes 'd 'g ] \plet{ 1/1 } \plet{ 4/6 } [ 'f 'd bes g f d ] \plet{ 1/1 } |
%64
	\duration{8}
	[ c g ] g4 |
% \meter{6/8} ||
%65
	[ g g g ] g4.  |
%66
	[ c c c ] c4.  |
%67
	[ g g g ] g4.  |
%68
	[ b8 b  b ] b4.  |
%69
	[ 'c g es ] 'c4.  |
%70
	'c4. 'es4.  |
%71
	[ 'f 'd b ] [ g a8 b ] |
%72
	'c r4 r4. |
% \meter{2/4} ||
%73
	f r f r |
%74
	es r 'c r |
%75
	b r b r |
%76
	'c r 'c4 |
%77
	\duration{16}
	\plet{ 4/6 } [ as f d f as 'd ] \plet{ 1/1 } \plet{ 4/6 } [ as f d `as d f ] \plet{ 1/1 } |
%78
	\plet{ 4/6 } [ g es c `g c es ] \plet{ 1/1 } \plet{ 4/6 } [ f 'c 'es 'c g es ] \plet{ 1/1 } |
%79
	\plet{ 4/6 } [ g d f b 'd 'g ] \plet{ 1/1 } \plet{ 4/6 } [ 'f 'd b g f d ] \plet{ 1/1 } |
%80
	[ c8 g8 ] g4 |
%81
	\duration{2}
	'c |
%82
	'c |
%83
	'des |
%84
	'c |
%85
	'c |
%86
	es |
%87
	d |
%88
	\textstyle "italic"
	a_"dacapo"
	\textstyle "roman"
% \key\G ||
	$
}
