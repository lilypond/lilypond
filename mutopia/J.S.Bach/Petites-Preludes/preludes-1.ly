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

  opus = "BWV 924";
  composer =	 "Johann Sebastian Bach (1685-1750)";
  enteredby =	 "jcn,hwn";

  % mutopia headers.
  mutopiatitle = "Prelude";
  mutopiacomposer = "J.S.Bach";
  mutopiaopus = "BWV924";
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

TODO: this file needs additional layouting: the trills look
differently in Lemoine, and the sharps should be below the trill.

Additionally, the performance of trills should be explained.

Lemoine puts fermatas on ending bar lines everywhere.

1.2.x versions of lily contain a version with dynamics and fingerings.

%}



\version "1.3.110";


lowstaff = \translator "Staff" = "lower"
upstaff = \translator "Staff" = "upper"

% upper = \context Staff \notes\relative c {
upper = \context Staff=upper \notes\relative c {
	\context Voice=i
	\property Voice . textStyle = "italic"

	r16 g'' c e r g, c e 
	r g, c d r b d g |
	r16 a, d  g r a, d f 
	r a, d e r c e a |
	r16 b, e a r b, e g 
	r c, e f r g, d' f |

	r g, d' e r g, c e 
	r a, c d r e, b' d |
	r e, b' c r e, a c 
	r f, a bes r c, g' bes |
	r c, g' a r c, f a 
	r d, f b r e, g c |
	r d, g c r d, g b 
	r c, g' b r c, fis a |
	r b, d a' r b, d g 
	r a, c g' r a, c f! |
	
	% ugh arpeggio
	<f4 d b4>
	r4
	
	\stemBoth 
	\stemUp
	r16 g,, b d  
	[ f \upstaff \stemDown g16 b d ] | 
	\stemUp b
	\lowstaff \stemDown
	[ g b d ] f
	\upstaff 
	[ g16 b d ] b
	[ g b d ]
	\stemUp

	% urg, below translator change affects previous beam too!?
	% howto separate translator command from previous beam end?
	\upstaff f g b f |
	\stemBoth
	e c' g f  e c' g e 
	d c' f, e  d b' f d |
	c b' e, d  c a' e c 
	b a' d, c  b g' d b |
	a g' c, b  a fis' c a 
	b f' d c  b f' d b |
	g e' c b  a e' c a 
	fis d' b a  g d' b g |
	e c' a g  fis c' a fis 
	d c' b a  b g d' f, |
	g e f d'  c b a g 
	f' d es c  fis, es' d c |
	b d b g  as f g d 
	es fis a c 
	< 
		{ \stemUp r c8 b16 }
		\context Voice=ii { \stemDown  d,8 f }
	>
	\stemBoth |
	<c1 g e>
	\bar "|.";
}

% lower = \context Staff \notes\relative c{
lower = \context Staff=lower \notes\relative c{
	\context Voice=i
	\property Voice . textStyle = "roman"
	c4 e g^"\\textsharp"-\mordent g,4 |
	d'4-\mordent f a-\mordent^"\\textsharp" a,4 |
 	e' e'-\mordent a, b-\upprall |
 	c4 e, fis gis-\upprall |
	a4 c, d e-\upprall |
	f4 e d-\prall c |
	g'-\mordent^"\\textsharp" g, g'-\mordent g, |
	g'-\mordent g, g'-\mordent g, |
	g' r s s | s s s s \clef "bass"; |
	<
		{ \stemUp g1 ~ g ~ g ~ g ~ g ~ g ~ g }
		\context Voice=ii { \stemDown g,1 ~ g ~ g ~ g ~ g ~ g ~ g }
	>
	<c,1 c,>
	\bar "|.";
}

global = \notes{
	\time 4/4;
}

\score{
	% Moderato
	\context PianoStaff <
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
	\paper { linewidth = 18.0 \cm;  }
	\midi{ \tempo 4 = 80; }
	\header {
		opus = "BWV 924";
		piece = "1";
	}
}
