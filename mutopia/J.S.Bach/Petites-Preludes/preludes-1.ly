\header{
filename =	 "preludes-1.ly";
% mmm
% title =	 "DOUZE PETITES PR\\'ELUDES";
opus = "BWV 924";
% piece = "\\numfont 1";
piece = "1";
% blz 1
% Clavierb"uchlein f"ur W. Fr. Bach
% Clav. W. Fr. Bach: 2-Praeambulum I
% ca 1720
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "jcn";
copyright =	 "public domain";
}

\version "1.0.7";

one = \notes\relative c {
	\property Voice . textstyle =  "italic"
	r16\p_"legato"\< [g''-1( c-3 )e-5] r [g,-1( c )e]
	r [g,( c )d] r [b-1( d-2 \!)g-5] |
	r\mf [a,( d-2 ) g-5] r [a,(\< d )f-4]
	r [a,( d )e-3] r [c-1( e-2 \!)a-5] |
	r\f [b, e a-5] r [b, e-3 g-5] 
	r [c,-1 e-3 f-4] r [g, d'-3 f-5] |
	r [g,-"poco a poco dim." d'-4 e] r [g, c-2 e]
	r [a,-1 c-3 d-4] r [e, b'-3 d-5] |
	r [e, b' c-4] r [e, a-2 c]
	r [f,-1 a-3 bes-4] r [c, g'-3 bes-5] |
	r [c, g' a-4] r [c, f-2 a] 
	r [d, f-2 b-5] r [e, g-2 c-5] |
	r\p [d, g c-5] r [d, g b-4]
	r [c, g'-3 b-5] r [c, fis-2 a-4] |
	r [b, d a'-5] r [b, d g-4]
	r [a, c g'-5] r [a, c f!-4] |
	% ugh arpeggio
	<
		{ \voiceone  f4 r s }
		{ \voicetwo  <g,4 d'> r s }
	>
	\onevoice 
	% ugh beam across staffs slur starts at lower staff
	r16 [g,-1( b d] |
	\voiceone 
	)b s4 [g'16-1( b-2 d] )b s s s [f'-2 g-3 b-5 f-2] |
	\onevoice 
	[e-1\f c'-5( g-3 f-2] [e-1 c'-5 g-3 e-2] 
	[)d c'-5( f,-3 e-2] [d-1 b'-5 f-3 d-2] |
	[)c b'-5( e,-3 d-2] [c-1 a'-5 e-3 c-2] 
	[)b-1 a'-5( d,-3 c-2] [b-1 g'-5 d-3 b-2] |
	[)a g'-5 c,-4 b] [a-"poco a poco dim." fis'-5 c-2 a-1]
	[b-2 f' d-4 c] [b f' d-4 b-2] |
	[g e' c-3 b] [a e'-5 c-3 a-1]
	[fis-2 d' b-3 a] [g-1 d'-5 b-4 g-2] |
	[e c'-5 a-4 g-3] [fis-2 c'-5 a-4 fis-2] 
	[d c'-5 b-4 a] [b-4 g-2 d'-5 f,-1] |
	[g-4 e-2 f-1 d'-5] [c-4 b a g]
	[f'-5 d-3 es-4 c-2] [fis,-1 es'-4 d-3 c-2] |
	[b-1 d-5 b-3 g-1] [as-4-"cresc. e rall." f-2 g-3 d-1]
	[es-2 fis-3 a-4 c-5] 
	< 
		{ \voiceone  r [c8 b16] }
		{ \voicetwo  [d,8 f-2] }
	>
	\onevoice  |
	<c1\mf g e>
	\bar "|.";
}

two = \notes\relative c{
	\property Voice . textstyle =  "roman"
	c4-5 e-3 g-\mordent^"(\\textsharp)" g,4 |
	d'-\mordent-5 f-3 a-\mordent^"(\\textsharp)" a,4 |
 	e'-5 e'-\mordent-1 a,-4 b-\upprall |
 	c4 e,-4 fis gis-\downprall |
	a4 c,-4 d e-\upprall |
	f4-1 e d-3-\prall c |
	g'-\mordent^"(\\textsharp)" g, g'-\mordent g, |
	g'-\mordent g, g'-\mordent g, |
	% ugh f should be at upper staff
	g'-\mordent r r16 [g,-5( b-4 d] )f-1 s s s |
	s [g-5( b-4 d-2] )f-1 s s s s \clef "violin"; [g-4 b-2 d-1] 
	s s s s \clef bass; |
	<
		{ \voiceone  g,,1 ~ g ~ g ~ g ~ g ~ g ~ g }
		{ \voicetwo  g,1 ~ g ~ g ~ g ~ g ~ g ~ g }
	>
	<c,1 c,>
	\bar "|.";
}

global  = \notes{
	\time 4/4;
}

treble_staff = \type Staff = treble <
	\global
	\one
>

bass_staff = \type Staff = bass <
% bass = Staff <
	\clef "bass";
	\global
	\two
>

grand_staff = \type GrandStaff <
	\treble_staff
	\bass_staff
>

\score{
        % Moderato
	\grand_staff
	\include "preludes-paper.ly";
	\midi{ \tempo 4 = 80; }
}
