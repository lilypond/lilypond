%{

 Six Petits Preludes,
 Collection Johann Peter Kellner
 ca 1703 - 1707



 Kellner was a student of Bach's.  
 
%}

\header{
  copyright =	 "public domain";
  source = "Ed. Henry Lemoine Urtext";
  title =	 "Pr\\\"aludum";
  composer =	 "Johann Sebastian Bach (1685-1750)";
  enteredby =	 "jcn,hwn";

  
  opus= "393";
  % mutopia headers.
  mutopiatitle = "Prelude";
  mutopiacomposer = "J.S.Bach";
  mutopiaopus = "BWV939";
  mutopiainstrument = "Piano";
  style = "baroque";
  copyright = "Public Domain";

  maintainer = "Jan Nieuwenhuizen";
  maintainer_email = "janneke@gnu.org";
  tagline =    "\\\\This music is part of the Mutopia project, http://sca.uwaterloo.ca/Mutopia/\\\\It has been typeset and placed in the public domain by " + \maintainer + ".\\\\Unrestricted modification and redistribution is permitted and encouraged - copy this music and share it!";
  maintainer = "janneke@gnu.org";
  lastupdated = "1999/Nov/14";
}



%{
Old versions of LilyPond include this prelude with dynamics and fingerings.
%}



\version "1.3.59";
upper = \context Staff \notes\relative c{
	\context Voice=i
	\property Voice . textStyle = "italic"
	r8 c' e g  e c bes' g |
	a c, f a  f c c' a |
	b g b d  b g f' d |
	<e2 c g>
	r4
	<e c g >
	<d2 c a>
	r4
	<d c a>
	< { \stemup [d8 g, 8 b d] }
	  \context Voice = ii < \stemdown b g >
	>
	
	<b4 g>
	<e b>
	c2
	r4
	<
		{ \stemup c ~ c8} 
		\context Voice=ii { \stemdown <e,4 a> ~ <e8 a> }
	>
	\stemboth 
	d, fis a  fis d c' a |
	b g b d  b g f'! d |
	e g, c e  c g g' e |
	fis a, c fis  c a a' fis |
	<g2 d b>
	r4 b,4 |
	c8 e g c  g e bes' g |
	a16 g f e  d c b ! a 
	b g a b  c d e f |
	g4
	<g,2 c>
	<d4 b'>
	<e1 c'>
	\bar "|.";
}

lower = \context Staff \notes\relative c{
	\context Voice=i
	<
		{ \stemup c1 ~ | c ~ | c }
		\context Voice=ii { \stemdown c,1 ~ | c ~ | c }
	>
	\stemboth |
	r8 c e g  e c c' e, |
	fis d fis a  fis d d' fis, |
	g2 r4 g |
	a8 a, c e  c a g' e |
	fis2 r4 d |
	g-\mordent-"(\\textsharp)" g, 4 g'-\mordent g, |
	% mordents in brackets...
	g'-\mordent g, g'-\mordent g, |
	g'-\mordent g, g'-\mordent g, |
	g'8 g, b d  b g f'! d |
	e c e g  e c c' e, |

	f1 ~ |
	f8 d e c g'4 g, |
	c,1
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
	  linewidth = 18.0 \cm;  
        }
	\midi{ \tempo 4 = 100; }
	\header {
		opus = "BWV 939";
		piece = "2";
	}
}

