% mlvio2.ly
% 
% Viola Quartet
% Martien Lohman (194x-????) 
%
% Violin II
% 
% quite alive at the time of writing; 
% copyright by ml
%
% The purpose of this file is to demonstrate features of LilyPond;
% respect the copyright.
%
violin2 = music { 
	$
	\octave{}
	\duration{4}
%1
	d2 |
%2
	[ `b8. `a16 ] `g4 |
%3
	[ g8 e8 ] fis |
%4
	d2 |
%5=1
	e2 |
%6=2
	[ `b8. `a16 ] `g4 |
%7
	`g `a |
%8
	`b `a |
%9=1
	d2 |
%10
	`b `g |
%11=4
	`g2 |
%12
	[ d8 e16 fis16 ] g |
%13
	fis g |
%14=10
	fis d |
%15
	c d |
%16
	\textstyle "italic"
	d2_"fine" |
	\textstyle "roman"
% \newline
%17=1
	d2 |
%18=2
	[ `b8. `a16 ] `g4 |
%19=3
	[ g8 e8 ] fis |
%20=4
	d2 |
%21=5=1
	e2 |
%22=6=2
	[ `b8. `a16 ] `g4 |
%23=4
	`g2 |
%24
	cis e |
%25
	a2 |
%first modulation
%26
	fis d |
%27
	d2 |
%28
	[ cis8 e8 ] [ d8 a8 ] |
%29
	[ a8 e8 ] [ a8 fis8 ] |
%30
	e fis |
%31
	g fis |
%32
	a fis |
%variant a
%33
	\duration{16}
	[ g8 b b ] [ b8 g a ] |
%34
	[ b8 b b ] f4 |
%35
	[ g8 e e ] [ e8 g g ] |
%36
	 b4 fis4 |
%37
	[ a8 a a ] [ a8 a a ] |
%38
	[ fis8 fis fis ] [ fis8 fis fis ] |
%39
	[ e8 e e ] [ e8 e e ] |
%40
	fis4 g4 |
%41
	\duration{8}
	\textstyle "italic"
	r_"pizz"
	\textstyle "roman"
	`b r `b |
%42
	r `b `g4 |
%43
	r fis r fis |
%44
	r g e4 |
%45
	r e r g |
%46
	r fis [ fis g ] |
%47
	r g r e |
%48
	r fis [ fis d ] |
%theme 2
%49
	\textstyle "italic"
	r4_"arco"
	\textstyle "roman"
	\plet{ 2/3 } [ d8 d d ] \plet{ 1/1 } |
%50
	r4 \plet{ 2/3 } [ g8 g g ] \plet{ 1/1 } |
%51
	d2 |
%52
	c2 |
%53
	r4 \plet{ 2/3 } [ d8 d d ] \plet{ 1/1 } |
%54
	 r4 \plet{ 2/3 } [ e e e ] \plet{ 1/1 } |
%55
	d2 |
%56
	f4 e4 |
% \key\Es ||
%57
	d r `b r |
%58
	`g r es r |
%59
	g r g r |
%60
	g r es4 |
%61
	f2 |
%62
	es2 |
%63
	`b d4.  |
%64
	es2 |
% \meter{6/8} ||
%65
	\duration{8}
	[ c c c ] c4.  |
%66
	[ `g `g `g ] `g4.  |
%67
	[ `b8 `b `b ] `b4.  |
%68
	[ g g g ] g4.  |
%69
	es2. |
%70
	[ c es g ] 'c4.  |
%71
	b4 g4 d4 |
%72
	es r4 r4. |
%73
% \meter{2/4} ||
	d r `b r |
%74
	`g r es r |
%75
	 g r g r |
%76
	 g r es4 |
%77
	\duration{2}
	f |
%78
	es |
%79
	`b8 d4.  |
%80
	es |
%81
	es |
%82
	es |
%83
	bes |
%84
	f |
%85
	f |
%86
	ges |
%87
%	= g |
%88
	\textstyle "italic"
	fis_"dacapo" |
	\textstyle "roman"
% \key\G ||
	$
}
