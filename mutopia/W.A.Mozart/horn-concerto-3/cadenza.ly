\header{
  title =	 "Konzert Nr. 3 Es dur";
  subtitle = "Cadenza ad lib.";
  enteredby =	 "HWN";
  copyright = "public domain";
  instrument = "Horn in Es";

  mutopiatitle =	 "Cadenza for horn concerto no. 3";
  mutopiacomposer = "Anon.";
  style = "classical";
  tagline =    "\\\\This music is part of the Mutopia project, http://sca.uwaterloo.ca/Mutopia/\\\\It has been typeset and placed in the public domain by Han-Wen Nienhuys.\\\\Unrestricted modification and redistribution is permitted and encouraged - copy this music and share it!";
  maintainer = "hanwen@cs.uu.nl";
  lastupdated = "1999/Oct/16";
  source = "Breitkopf & Haertel + own modifications";
}



%{

This is a cadenza to Mozart 3, as given to me by Arthur Verhofstad
(one of my teachers). The source is unknown

Lots more can be found in Uijlenspiegel 1998 no. 1, in an article by
Herman Jeurissen.

%}

\version "1.2.13";


cad = \notes  \relative c' {
	\property Score.midiInstrument = "french horn"
	\context Staff {
	\emptyText
	\cadenza 1;
	

	\clef "violin";
	c'4.\mf g8


	[e'^"accel" () d  c b]
	[b() c] g-\fermata
		\bar "empty";
			c, ~ [c^\turn_"rubato" e g c]
	e4. e8 [g () f_"rit" e d]

	dis4() e4
		\bar "" ;
		r8 [c16 d] [e f g gis]

	a4-> f,() e g'
	f-> d,() cis e'

	d4^\fermata
		\bar "" ;

	r8 a [b cis]
	\grace { e8( }
	[d16 cis d e]
	f4 ~ [f16 e d c]
	b4-\turn
	\times 2/3 { [ d8 c8 a8] }
	g2~
		\bar "" ;
	[g16 c, e g] [c e, g c]
	[e g, c e] g4^\fermata 
		\bar "" ;
	[g8.(_"a tempo" e16 g8. )e16]
	a4. g8 [f8 e8 d8 c8]
	g2 d'2-\trill
	\grace { [c32 d] }
	 c4
	} }
	
\score {
	\notes { \cad }
%	\midi { \tempo 4 = 90; }
	\paper {  casting_algorithm = \Wordwrap;}

}
