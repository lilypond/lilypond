%{

 Six Petits Preludes,
 Collection Johann Peter Kellner
 ca 1703 - 1707



 Kellner was a student of Bach's.  
 
%}

\header{
  copyright =	 "public domain";
  source = "Ed. Henry Lemoine Urtext";


  composer =	 "Johann Sebastian Bach (1685-1750)";
  enteredby =	 "jcn,hwn";
  copyright =	 "public domain";

  % mutopia headers.
  mutopiatitle = "Prelude";
  mutopiacomposer = "J.S.Bach";
  mutopiainstrument = "Piano";
  style = "baroque";
  copyright = "Public Domain";
  tagline =    "\\\\This music is part of the Mutopia project, http://sca.uwaterloo.ca/Mutopia/\\\\It has been typeset and placed in the public domain by Jan Nieuwenhuizen.\\\\Unrestricted modification and redistribution is permitted and encouraged - copy this music and share it!";
  maintainer = "janneke@gnu.org";
  lastupdated = "1999/Nov/8";
}

%{
Old versions of LilyPond include this prelude with dynamics and fingerings.

%}



\version "1.3.4";
upper = \context Staff \notes\relative c{
	\context Voice=i
	\property Voice . textStyle = "italic"
	r8 c'( e g  e c bes' g |
	)a c,( f a  f c c' a |
	)b g( b d  b g f' d |
	<
		{\stemup )e2 r4 e }
		\context Voice = ii {\stemdown <c2 g > r4 <c g > }
	>
	\stemboth |
	< 
		 { \stemup d2 r4 d }
		 \context Voice = ii { \stemdown <c2 a > r4 <c a > }
	>
	\stemboth |
	< 
		 { \stemup d8 }
		 \context Voice = ii { \stemdown <b 8 g > }
	>
	\stemboth 
	g, 8( b d 
	<
		{ \stemup )b 4 e }
		\context Voice = ii { \stemdown g, 4 b }
	>
	\stemboth |
	<
		{ \stemup c2 r4 < {c ~ c8} {a 4 ~ a 8} > }
		\context Voice=ii { \stemdown c2 r4 e, 4 ~ e 8 }
	>
	\stemboth 
	d,( fis a  fis d c' a |
	)b g( b d  b g f'! d |
	)e g,( c e  c g g' e |
	)fis a,( c fis  c a a' fis |
	<
		{ \stemup )g2 }
		\context Voice = ii { \stemdown <d2 b > }
	>
	\stemboth 
	r4 b4( |
	c,8 e g c  g e bes' g |
	)a16-> g f e  d c b ! a 
	b g a b  c d e f |
	g4
	<
		{ \stemup c,2 b 4 }
		\context Voice=ii { \stemdown g 2 d 4 }
	>
	\stemboth 
	<
		{ \stemup c1^5 }
		\context Voice=ii { \stemdown e, 1_1}
	>
	\stemboth 
	\bar "|.";
}

lower = \context Staff \notes\relative c{
	\context Voice=i
	<
		{ \stemup c1 ~ | c ~ | c }
		\context Voice=ii { \stemdown c,1 ~ | c ~ | c }
	>
	\stemboth |
	r8 c( e g  e c c' e, |
	)fis d( fis a  fis d d' fis, |
	)g2 r4 g( |
	)a8 a,( c e  c a g' e |
	)fis2 r4 d |
	g-\mordent-"(\\textsharp)" g, 4 g'-\mordent g, |
	% mordents in brackets...
	g'-\mordent g, g'-\mordent g, |
	g'-\mordent g, g'-\mordent g, |
	g'8 g,( b d  b g f'! d |
	)e c e g  e c c' e, |
	\property Voice . textStyle = "finger"
	f1^"3\\_1" ~ |
	f8 d e c g'4 g,^"5\\_2" |
	<c1 c,1>
	\bar "|.";
}

global = \notes{
	\time 4/4;
}

\score{
	\context GrandStaff <
		\context Staff = upper <
			\global
			\upper
		>
		\context Staff = lower <
			\global
			\clef "bass";
			\lower
		>
	>
	\paper{
	}
	\midi{ \tempo 4 = 100; }
	\header {
		opus = "BWV 939";
		piece = "2";
	}
}

