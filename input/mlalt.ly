% mlalt.ly
% 
% Viola Quartet
% Martien Lohman (194x-????) 
%
% Alto I
% 
% quite alive at the time of writing; 
% copyright by ml
%
% The purpose of this file is to demonstrate features of LilyPond;
% respect the copyright.
%
alto = \music { 
	
	\duration{16}
	\clef\alto
	\octave{}
	\key{fis}
%1
        [ 'b8. 'a ] [ 'g8 'b c ] |
%2
	[ d8 g8 ] d4 |
%3
	[ e fis g a ] d4 |
%4
	[ c 'b 'a 'b ] 'b4 |
%5=1
	[ 'b8. 'a ] [ 'g8 'b c ] |
%6=2
	[ d8 g8 ] d4 |
%7
	[ c 'b 'a 'g ] [ 'fis 'e 'd 'c ] |
%8
	[ 'd8. 'e ] [ 'd 'fis 'a c ] |
%9=1
	[ 'b8. 'a ] [ 'g8 'b c ] |
%10
	[ d8 'd8 ] d4 |
%11=4
	[ c 'b 'a 'b ] 'b4 |
%12
	[ 'a8 d8 ] 'b4 |
%13
	[ d8. c ] [ 'b8 'a 'g ] |
%14=10
	[ d8 'd8 ] d4 |
%15
	[ e8 fis8 ] [ g8 d8 ] |
%16
	\textstyle "italic"
	[ c 'a 'fis 'd ] 'g4_"fine" |
	\textstyle "roman"
% \scoreverb{\mulooseness=-1}
% \newline
%17
%=1
	[ 'b8. 'a ] [ 'g8 'b c ] |
%18=2
	[ d8 g8 ] d4 |
%19=3
	[ e fis g a ] d4 |
%20=4
	[ c 'b 'a 'b ] 'b4 |
%21=5=1
	[ 'b8. 'a ] [ 'g8 'b c ] |
%22=6=2
	[ d8 g8 ] d4 |
%23=20=4
	[ c 'b 'a 'b ] 'b4 |
%24
	[ 'a8. 'g ] [ 'a cis e g ] |
%first modulation
%25
	[ fis8. e ] [ d8 fis g ] |
%26
	[ a8 'a8 ] a4 |
%27
	[ g fis e fis ] fis4 |
%28
	[ e8 a8 ] fis4 |
%29
	[ a8. g ] [ fis8 e d ] |
%30
	[ a8 'a8 ] a4 |
%31
	[ b8 cis'8 ] [ d'8 a8 ] |
%32
	[ g e cis 'a ] d4 |
%variant a
%33
	[ 'b8 'g 'g ] [ 'g8 'b c ] |
%34
	[ d8 'g 'g ] 'g4 |
%35
	[ e8 'g 'g ] [ 'g8 e fis ] |
%36
	[ g8. fis32 e32 ] d4 |
%37
	[ e8 c c ] [ c8 'b 'a ] |
%38
	[ d8 'b 'b ] [ 'b8 'a 'g ] |
%39
	[ c8 'a 'a ] [ 'a8 'b 'a ] |
%40
        'a4 'g4 |
%41
	[ 'g8 'a 'g ] [ d8 e d ] |
%42
	[ 'g8 'a 'g ] [ g8 e8 ] |
%43
	[ d8 e d ] [ d8 c 'b ] |
%44
	[ 'b8. c32 'b32 ] 'a4 |
%45
	[ 'g 'a 'g 'a 'g 'a ]4/6   [ 'g 'b c d e fis ]4/6  |
%46
	[ g fis e d 'b 'g ]4/6  [ 'a8 'g8 ] |
%47
	[ g fis e d c 'b ]4/6   [ e d c 'b 'a 'g ]4/6  |
%48
	[ d e d d 'b 'g ]4/6  [ 'a8 'g8 ] |
%theme 2
	\duration{8}
%49
	[ f e d ]2/3  f4 |
%50
	[ e d c ]2/3  e4 |
%51
	[ d g 'g ]2/3   [ 'g 'a 'b ]2/3  |
%52
        d4 e4 |
%53
	[ f e d ]2/3  f4 |
%54
	[ e d c ]2/3  e4 |
%55
	[ d g 'g ]2/3   [ 'g 'a 'b ]2/3  |
%56
        'b4 c4 \key{bes es as} | % || \key\Es
%57
	[ 'b d ] [ 'g 'b ] |
%58
	[ c es ] [ 'g c ] |
%59
	[ d f ] [ 'g f ] |
%60
	[ es16 d16 es16 f16 ] g4 |
%61
	[ as f ] [ d as ] |
%62
	[ g es ] [ c 'as ] |
%63
	[ 'g 'b ] [ es d ] |
%64
	[ c8. 'g32 'es32 ] 'c4 | % || \meter{6/8}
%65
	es4.( [ )es d c ] |
%66
        es2.  |
%67
	d4.( [ )d c 'b ] |
%68
        d2.  |
%69
%	g4.\stemdown( [ ) g as g ] |
	g4.( [ ) g as g ] |
%70
%	g4.\stemdown( [ ) g f es ] |
	g4.( [ )g f es ] |
%71
        d2.  |
%72
        c r4 r4. | % || \meter{2/4}
%73
	\duration{8}
	[ 'b d ] [ 'g 'b ] |
%74
	[ c es ] [ 'g c ] |
%75
	[ d f ] [ 'g f ] |
%76
	[ es16 d16 es16 f16 ] g4 |
%77
	[ as f ] [ d as ] |
%78
	[ g es ] [ c 'as ] |
%79
	[ 'g 'b ] [ es d ] |
%80
	[ c8. 'g32 'es32 ] 'c4 |
%81
        g4 [ as g ] |
%82
	\duration{16}
	[ g f es f ] g4 |
%83
	[ g f e f ] g4 |
%84
	[ as g f g ] as4 |
%85
        f4 [ f des c 'bes ] |
%86
        'a2 |
%87
        'b2 |
%88
	\textstyle "italic"
        c2_"dacapo" \key{fis} |
	\textstyle "roman"
%\scoreverb{\mulooseness=1}
%	\key\G ||
	
}
