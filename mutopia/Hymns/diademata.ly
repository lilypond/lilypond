\header {
filename = "diademata.ly";
enteredby = "Peter Chubb";
composer = "Sir G. J. Elvey";
date = "1868";
title = "Diademata";
metre = "6 6. 8 6. D";
}
\version "1.0.16";

sop=\notes \transpose c''{
	ees2  | ees4 ees4 g2 g2 |  c'1.  \bar "||";
	c'2 | bes2 ees aes g |  f1.  \bar "||";
	f2 | g bes c' bes |  a g4 ~ f  bes2  
	ees'2 | d' ees' c' c' |  bes1.  \bar "||";
	bes2 | bes g f ees |  c'1.  \bar "||";
	c'2 | c' a g f |  d'1.  \bar "||";
	d'2 | ees'2. d'4 c'2 bes |  aes f g 
	bes2 | aes g f f | ees\breve \bar "||";

}

alt=\notes \transpose c''{
	bes,2 bes,4 bes,4 ees2 ees2 |  ees1.  \bar "||";
	ees2 | ees ees d ees |  d1.  \bar "||";
	d2 | ees f ees g |  ees c f 
	ees2 | f g c f |  d1.  \bar "||";
	d2 | ees ees d ees |  ees1.  \bar "||";
	e2 | f f ees ees |  d1.  \bar "||";
	f2 | ees ees ees ees |  ees d ees 
	ees | ees ees ees d | ees\breve  \bar "||";
}


ten=\notes \transpose c' {
	g2  g4 g4 g2 g2 |  aes1.  \bar "||";
	aes2 | bes c' aes bes |  bes1.  \bar "||";
	bes2 | bes bes g g  |  c' a bes 
	a2 | bes bes bes a |  bes1.  \bar "||";
	bes2 | bes bes aes bes |  c'1.  \bar "||";
	bes2 | a c' bes c' |  bes1.  \bar "||";
	bes2 | bes bes aes bes |  c' bes bes 
	bes c' bes bes2. aes4 | g\breve  \bar "||";
}

bass=\notes \transpose c' {
	ees2 ees4 ees4 c2 c2 |  aes,1.  \bar "||";
	aes2 | g aes f ees |  bes,1.  \bar "||";
	bes,2 | ees d c ees |  f ees d 
	c2 bes, ees f f |  bes,1.  \bar "||";
	aes2 | g ees f g |  aes1.  \bar "||";
	g2 | f f g a |  bes1.  \bar "||";
	aes2 | g g  aes g | f bes ees 
	g,2 | aes, ees bes, bes, | ees\breve  \bar "||";
}

global = \notes{
	\time 4/2;
	\property Staff.timeSignatureStyle="none"
	\key ees;
}

$upper_staff = \context Staff = upper {
	\global
	\clef "treble";
	\context Staff <
		{ \voiceone \sop }
		{ \voicetwo \alt }
	>
}

$lower_staff = \context Staff = lower {
	\global
	\clef "bass";
	\context Staff <
		{ \voiceone \ten }
		{ \voicetwo \bass }
	>
}

\score{
	\context ChoirStaff\notes	<
		\$upper_staff
		\$lower_staff
	>
	\paper {
		linewidth= 140.0\mm;
		gourlay_maxmeasures = 6.0;
	}
}
