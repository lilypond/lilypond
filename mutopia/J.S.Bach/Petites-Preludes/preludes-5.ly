%{

 Six Petits Preludes,
 Collection Johann Peter Kellner
 ca 1703 - 1707

 Kellner was a student of Bach's that copied some 90 works of his master.
 
%}

\header{
  copyright =	 "public domain";
  source = "Ed. Henry Lemoine Urtext";
  title =	 "Pr\\\"aludum";
  opus = "BWV 926";
  composer =	 "Johann Sebastian Bach (1685-1750)";
  enteredby =	 "jcn,hwn";

  % mutopia headers.
  mutopiatitle = "Prelude";
  mutopiacomposer = "J.S.Bach";
  mutopiaopus = "BWV926";
  mutopiainstrument = "Piano";
  style = "baroque";
  copyright = "Public Domain";
  maintainer = "Jan Nieuwenhuizen";
  maintainer_email = "janneke@gnu.org";
  tagline =    "\\\\This music is part of the Mutopia project, http://sca.uwaterloo.ca/Mutopia/\\\\It has been typeset and placed in the public domain by " + \maintainer + ".\\\\Unrestricted modification and redistribution is permitted and encouraged - copy this music and share it!";
  maintainer = "janneke@gnu.org";
  lastupdated = "1999/Nov/14";
  
}

\version "1.3.42";

upper = \context Staff \notes\relative c
	\context Voice=i {
	\property Voice . textStyle = "italic"


	[d'8 a' f d a' f] |
	d a' f d a' f |
	d bes' g d bes' g |
	d bes' g d bes' g |
	%5
	cis, g' e cis g' e |
	cis bes' g e a g |
	f d f a f a |
	d a d f d f |
	b  f e d c b |
	%10
	a gis fis e d' b |
	< 
		{
			\stemup 
			\property Voice . textStyle = "roman"
			c4-\mordent^"(\\textsharp)" r r |
			c4-\mordent^"(\\textsharp)" r r
			\property Voice . textStyle = "italic"
		}
		\context Voice=ii { \stemdown a4 r r | a4 r r }
	> |
	\stemboth 
	a'8 es d c bes a |
	g fis e! d c' a |
	%15
	bes8-\mordent d8 bes g g'4 |
	r8 d c bes a-\prall g |
	a c a f  f'4 |
	r8 c bes a g-\prall f |
	g bes a g f e |
	%20
	f d f a d g, |
	< 
		{ 
			\stemup 
			% ... textnatural
%			cis8-\prall e8-"poco cresc." cis a e' cis 
			cis8-\prall e8 cis a e' cis 
		}
		\context Voice=ii { \stemdown a4 }
	>
	\stemboth 
	a8 e' cis a bes! a |
	g e' cis g e' cis |
	g e' cis g a g |

	f d' bes f d' bes |
	%25
	f d' bes f d' bes |

	fis c' a fis c' a |
	fis c' a fis c' a |
	bes g fis g d g |
	bes g d bes' g d |
	%30
	e g fis g bes g |
	es bes' g es bes' g |
	cis, bes' g cis, bes' g |
	cis, bes' g cis, a' g |
	f a f d a' f |
	%35
	d a' f d cis d |
	e g e bes g' e |
	bes g' e cis a g' |
	f16 d c bes \stemup a s16 s8 s4 |
% ugh
%	s1 |
	s4 s4 s4 |
	%40
	s4 s16 [d16 f a] \stemdown [d, f a] \stemup d |
	\stemboth
	f a f d  f d b d  gis, b a gis |

	% arpeggio
	<g'!4. e a,> a8 
	< f4  d a > ~ 
	[f8 e]
	< { \stemup 
		e4.^\prall d8
		}
		\context Voice=ii { \stemdown cis2 }
	> 
	\stemboth 
	d8 c! a d bes g |
	%45
	c a fis bes g e  |

	a fis d g e cis |

	%  the mordent is on the D !
	<fis2. d-\mordent a> 
}


lower = \context Staff \notes\relative c{
	\context Voice=i

	d4-\mordent r r |
	d,4 r r |
	d'-\mordent r r |
	d,4 r r |
	d'-\mordent r r |

	d,4 r r |
	d'8 a d f d f |
	a f a d a d |
	gis,4 r r |
	e gis e |
	a8 e' c a e' c  |
	g! es' c g es' c  |
	fis,4 r r 
	d  fis d |
	\property Voice . textStyle = "roman"
	g4-\mordent^"(\\textsharp)" r r8 f!8 |
	\property Voice . textStyle = "italic"
	e4-\prall r r |
	f4-\mordent r r8 e |
	d4 r8 f e d |
	e d cis e d cis |
	d4 c! bes |
	a a' a, |
	a a' a, |
	a a' a, |
	a a' a, |
	bes r r |
	bes bes' bes, |
	a r r |
	a d d, |
	g r r |
	g g' g, |
	g r r |
	g g' g, |
	a r r |
	a a' a, |
	a r r |
	a a' a, |
	cis, r r |
	cis cis' cis, |
	d s16
	\stemdown
	[g'16 f e] f a d, f | 
	a, \stemup [d c bes] 
	a \stemdown [ g f e] d f a d |
	\stemup

	[f a]
	\stemdown
	[d, f] a s s8 s4-"*" |
	\stemboth
% ugh whole rest has duration of one bar
	R2. |
%	r4 r r |
	cis,8 e cis a  d16 c bes a |
	g8 g' a g a a, |
	d4 d' d, |
	d r r |
	d, d' d, |
	d2. |
}

global = \notes{
	\time 3/4;
	\key f;
}

\score{
	% Moderato
	\context PianoStaff <
		\context Staff = upper <
			\global
			{ \upper \bar "|."; }
		>
		\context Staff = lower <
			\global
			\clef "bass";
			\lower
		>
	>
	\paper{
		linewidth = 18.0 \cm;  

	}
	\midi{ \tempo 4 = 90; }
	\header{
		piece = "5";
		opus = "BWV 926";
	}
}

