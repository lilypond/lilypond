\header {
title = "Parties sur les fleut Dous \\'a 3";
composer = "Johann Christoph Faber";
piece = "1.  Overture";
}

\version "1.0.20";

global=\notes{
	\time 2/2;
	\key C;
}

treble=\notes\relative c''' {
%1
	g2 c4 g |
	g4( [f8 )e] f2 |
	f4( [e8 )d] e2 |
	[e8( )f g f] e4.( )d8 |
%5
	d2 r8 g8 [a( )e] |
	f2 r8 f8 [g( )d] |
	e2( [)e8 e( d )c] |
	d2( [)d8 d( c )b] |
	g'4. g8 g4 a |
%10
% implied trill on d
	d,4 e d4. c8 |
	c8 [g' c( )b] a2 |
	r8 a [d( )c] [b b( a )b] |
	[c( )b a g] fis4 [g8 a] |
	b2 [b8( )a g a] |
%15
	fis4. g8 g4 f |
	r8 g8 [f( )g] e4.( )d8 |
% Another implied trill
	d4 e d4. c8 |
	% \semibreve = \dottedminim
	\time 3/4;
	c4 g' g |
	[g8( )f e f g a] |
%20
	g4 e r |
	r2. |
	r4 g g |
	c [b8( )a g bes]  |
	a4 f r |
%25
	r2. |
	r4 a g |
	a f e |
	f [a8( )g f e] |
	d4 g g |
%30
	g( )a b |
	c e, d |
	e f g |
	a d, d |
	d( )e f |
%35
	g r4 r |
	r e f |
	g( [a8 )g f e ] |
	f4 e2 |
	d4 g( )f |
%40
	e r4 r4 |
	r g c |
	[a8( g )f g f e] |
	[ d8( )c d e f g ]|
	[e ( ) f e( f g )a ]|
%45
% Implied trill and turn on the D
	[f8( )e] d2 |
	c4 r4 r4 |
	e r4 r4 |
	g( )a b |
	c e, f |
%50
	g( [a8 )g f e] |
	d4.( )g8 g4 |
	[f8( )e] d4. c8 |
	c2. \bar "|.";
}

tenor=\notes\relative c'' {
	e2 g4 e |
	c2( [)c8 c8( b )a] |
	b2 c2 |
	[c8()d e d] c4.( )b8 |
%5
	b4 [c8 d] e2 |
	r8 c8 [d( )a] b2 |
	c2 a2( |
	[)a8 a8( g )f] g2 |
	c2. c4|
%10
	b c c( )b |
	c g' f2 |
	r8 f [a a] [g e(] )f4 |
	g e a, d |
	g2. e4 |
%15
	d a [b8. c16 c8. d16] |
	d2 c4.( )b8 |
	b4 c c( )b |
	% \semibreve = \dottedminim
	\time 3/4;
	c4 e e |
	[e8( )d c d e f ]|
%20
	e4 c r |
	r2. |
	r4 e e |
	a [g8( )f e g] |
	f4 c r4 |
%25
	r2. |
	r4 f e |
	f a, g |
	a [f'8( )e d c ] |
	b4 e d |
%30
	e( )f d |
	e c b |
	c c e |
	f b, a |
	b( )c d |
%35
	e r4 r4 |
	r c d |
	e e e |
	d d( )cis |
	d e( )d |
%40
	c r4 r4 |
	r4 e g |
	[f8( e )d e d c ] |
	[b8( )a b c d b] |
	[c8 ( ) d c( d e )f] |
%45
	[d( )c] c4 b |
	c r r
	c r r |
	e ( ) f d |
	e c d |
%50
	e( [f8 )e d c] |
	b4.( )e8 e4 |
	[d8 ()c] c4 b |
	c2. \bar "|.";
}

bass=\notes\relative c' {
	c4 [c,8 d] e4.( [f16 )g] |
	a2( [)a8 a( g )f] |
	g2 [c,8 f g()a] |
	c2 c, |
%5
	[g'8()f e d ] cis2 |
	d4. d8 g4. g,8 |
	[c8 g' c( )e, ] f2( |
	[)f8 f( e )d] e2 |
	e2. f4 |
%10
	g2 g, |
	c r8 c [f()e] |
	d2 g4 f |
	[e8()d] c4 [d8()c b a]|
	g4 a b c|
%15
	d2 [e8. c16 f8. d16] |
	g2 c, |
	f2 g |
	% \semibreve = \dottedminim
	\time 3/4;
	c,4 r r |
	r2. |
%20
	r4 c' a |
	c e, g |
	c, r r |
	r2.
	r4 f c |
%25
	f a, c |
	f, r r |
	r f' c |
	f r r |
	r c' g |
%30
	c f, g |
	c, e g |
	c, a' c |
	f, g fis |
	g e d |
%35
	c e g |
	c, e d |
	c cis cis |
	d a' a, |
	d r r |
%40
	r e g |
	c, c' e, |
	f2. |
	g2. |
	a4 [g8()f] e4 |
%45
	f g g, |
	c e g |
	c e, g |
	c, f g |
	c a [g8()f]
%50
	 e2 f4 |
	 g4()f e |
	  f g g, |
	 c2. \bar "|.";
}
	

\score{
	\context StaffGroup <
	      \context Staff =treble \notes {
		    \property Staff.Instrument = "Treble" 
		    \global \treble 
	      }

	      \context Staff =tenor \notes {
	      	      \property Staff.Instrument = "Tenor"
		      \global \tenor 
	      }

	      \context Staff =bass \notes {
	      	      \property Staff.Instrument = "Bass"
		      \global \clef "bass"; \bass
	      }
	>
	\paper {
		\translator { \BarNumberingStaffContext }
	}
}
	

