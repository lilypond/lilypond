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
violinI = \melodic  {
	\clef "violin";
	\octave c;
	\duration 4;
	\key fis;
%1
	g [ b8. a16 ] |
%2
	g2 |
%3
	g [ a16 b16 c'16 a16 ] |
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
	[ fis8 a8 ] [ g8 d'8 ] |
%13
	[ d'8 a8 ] [ d'8 b8 ] |
%14=10
	a b |
%15
	c' b |
%16
	\textstyle "italic";
	a g_"fine"
	\textstyle "roman" ;
	|
%\newline
%17=1
	g [ b8. a16 ] |
%18=2
	g2 |
%19=3
	g [ a16 b16 c'16 a16 ] |
%20=4
	g2 |
%21=5
	g [ b8. a16 ] |
%22=6
	g2 |
%23=4
	e [ d16 c16 'b16 c16 ] |
%24
	cis cis' |
%25
	d' [ fis'8. e'16 ] |
%first modulation
%26
	d' c' |
%27
	b a |
%28
	[ a8 b16 cis'16 ] d' |
%29
	cis' d' |
%30
	cis' a |
%31
	[ g8 d'8( ] ) d' |
%32
	cis' c' |
%variant a'
%33
	\duration 16; [ b8 d' d' ] [ d'8 d' d' ] |
%34
	[ g'8 d' d' ] b4 |
%35
	[ c'8 c' c' ] [ c'8 c' c' ] |
%36
	b2 |
%37
	[ c'8 e' e' ] [ e'8 d' c' ] |
%38
	[ b8 d' d' ] [ d'8 d' d' ] |
%39
	[ e'8 c' c' ] [ c'8 d' c' ] |
%40
	c'4 < b4 d'4 > |
%41
	\textstyle "italic";
	\duration 8;
	r_"pizz" d r g
	\textstyle "roman" ;|
%42
	r d e4 |
%43
	r b r d' |
%44
	r e' c'4 |
%45
	r b r b |
%46
	r b [ a b ] |
%47
	r b r c' |
%48
	r d' [ d' b ] |
%theme 2
%49
	\textstyle "italic";
	r4_"arco"  [ b8  b  b ]2/3 
	\textstyle "roman" ;|
%50
	r4  [ g g g ]2/3  |
%51
	b2 |
%52
	c'2 |
%53
	r4  [ b b b ]2/3  |
%54
	r4  [ g g g ]2/3  |
%55
	b2 |
%56
	g2 \key bes es as;|
% \key\Es ||
%57
	f r f r |
%58
	es r c' r |
%59
	b r b r |
%60
	c' r c'4 |
%61
	\duration 16;
	 [ as f d f as d' ]4/6   [ as f d 'as d f ]4/6  |
%62
	 [ g es c 'g c es ]4/6   [ g c' es' c' g es ]4/6  |
%63
	 [ g d g bes d' g' ]4/6   [ f' d' bes g f d ]4/6  |
%64
	\duration 8;
	[ c g ] g4 |
% \meter 6/8; ||
%65
	[ g g g ] g4.  |
%66
	[ c c c ] c4.  |
%67
	[ g g g ] g4.  |
%68
	[ b8 b  b ] b4.  |
%69
	[ c' g es ] c'4.  |
%70
	c'4. es'4.  |
%71
	[ f' d' b ] [ g a8 b ] |
%72
	c' r4 r4. |
% \meter 2/4; ||
%73
	f r f r |
%74
	es r c' r |
%75
	b r b r |
%76
	c' r c'4 |
%77
	\duration 16;
	 [ as f d f as d' ]4/6   [ as f d 'as d f ]4/6  |
%78
	 [ g es c 'g c es ]4/6   [ f c' es' c' g es ]4/6  |
%79
	 [ g d f b d' g' ]4/6   [ f' d' b g f d ]4/6  |
%80
	[ c8 g8 ] g4 |
%81
	\duration 2;
	c' |
%82
	c' |
%83
	des' |
%84
	c' |
%85
	c' |
%86
	es |
%87
	d |
%88
	\textstyle "italic";
	a_"dacapo"
	\textstyle "roman";
	\key fis; |
% \key\G ||
	
}
