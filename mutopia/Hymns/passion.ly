\header {
filename =  "passion.ly";
title =  "Passion Chorale";
composer =  "J. S. Bach";
enteredby =  "Peter Chubb";
}

\version "1.3.117";

sop=\notes \transpose c'' {
	\repeat "volta" 2 {
	\partial 2;
	e2 | a g f e | d1 e2 \breathe b | c' b4([c'8
	)d'] c'2 b4( )a4 |
	a1. }
	c'2 | b4( )a4 g2 a b | c'1 c'2\breathe g2 |
	a g a4( )g f2 | e1. \breathe
	c'2 | b4( )c'4 d'2 c' b | a1 b2\breathe 
	e2 | f e d g4( )f4 | e1. \bar "||";
}

alt=\notes \transpose c'' {
	\repeat "volta" 2  { 
	\partial 2;
	c2 | c4( )d4 e2 a,4( )b,4 c2 |  c2( )b, c
	e | e f e d4( )c4 |
	c1. }
	e2 | fis e f f |  g( )f e 
	e2 e4( )d cis( )d e2 e4(
	)d |  d2( )cis1 
	a2 | g g g4( )a b( )g | g2( )fis g
	c2 | d c d e4( )d | d2( c )b, \bar "||";
}


ten=\notes{
	\repeat "volta" 2 {
	\partial 2;
	a2 | a4( )b c'2 d' g |  a( )g g b |
	a a a gis |
	e1. }
	a2 | fis b c' d' | c'4( bes2 )aes4 g2  
	c'4( )bes |a2 bes a a | a1. 
	d'2 | d' d'4( [e'8 )f'8] e'2 d' | e'2( )d' d' 
	g2 | g g g4( )a bes2 |  b2( a )gis  \bar "||";
}

bass=\notes{
	\repeat "volta" 2 {	
	\partial 2;
	a4( )g | f2 e d c |  fis,( )g,	c
	gis,2 | a, d e e | 
	a,1.  }
	a2 | dis e ees d |  e( )f c 
	c | f e4( )d cis2 d | a,1. 
	fis2 | g4( )a b2 e4( )fis g2 | cis2( )d2 g, 
	c2 | b, c bes,4( )a, g,2 |  gis,2( a, )e  \bar "||";
}

global=\notes{
	\key c \major;
	\time 4/2;
	\property Staff.timeSignatureStyle="none"
}

\score{
	<
		\context Staff = "top" {
		\clef "treble";
		\global
		\context Staff <{\voiceOne \sop}{\voiceTwo \alt} >
		}
		\context Staff = "bottom" {
		\clef "bass";
		\global
		\context Staff <  {\voiceOne \ten}{\voiceTwo \bass} >
		}
	>

      \paper {
	linewidth= 140.\mm;

	gourlay_maxmeasures = 6.;
      }
}
