\header {
  filename = "diademata.ly";
  enteredby = "Peter Chubb";
  composer = "Sir G. J. Elvey";
  date = "1868";
  title = "Diademata";
  metre = "6 6. 8 6. D";
}

\version "1.1.52";

sop=\notes \transpose c''{
	ees2  ees4 ees4 g2 g2 |  c'1.
	c'2 | bes2 ees aes g |  f1.
	f2 | g bes c' bes |  a g4( )f  bes2  
	ees'2 | d' ees' c' c' |  bes1.
	bes2 | bes g f ees |  c'1.
	c'2 | c' a g f |  d'1.
	d'2 | ees'2. d'4 c'2 bes |  aes f g 
	bes2 | aes g f f | ees\breve

}

alt=\notes \transpose c''{
	bes,2 bes,4 bes,4 ees2 ees2 |  ees1.  
	ees2 | ees ees d ees |  d1.  
	d2 | ees f ees g |  ees c f 
	ees2 | f g c f |  d1.  
	d2 | ees ees d ees |  ees1.  
	e2 | f f ees ees |  d1.  
	f2 | ees ees ees ees |  ees d ees 
	ees | ees ees ees d | ees\breve  
}


ten=\notes \transpose c' {
	g2  g4 g4 g2 g2 |  aes1.  
	aes2 | bes c' aes bes |  bes1.  
	bes2 | bes bes g g  |  c' a bes 
	a2 | bes bes bes a |  bes1.  
	bes2 | bes bes aes bes |  c'1.  
	bes2 | a c' bes c' |  bes1.  
	bes2 | bes bes aes bes |  c' bes bes 
	bes c' bes bes2. aes4 | g\breve  
}

bass=\notes \transpose c' {
	ees2 ees4 ees4 c2 c2 |  aes,1.
	aes2 | g aes f ees |  bes,1.  
	bes,2 | ees d c ees |  f ees d 
	c2 bes, ees f f |  bes,1.  
	aes2 | g ees f g |  aes1.  
	g2 | f f g a |  bes1.  
	aes2 | g g  aes g | f bes ees 
	g,2 | aes, ees bes, bes, | ees\breve
}

global = \notes{
	\time 4/2;
	\key ees;
	\skip 2*4; \skip 1.; \bar "||";
	\skip 2; \skip 2*4; \skip 1.; \bar "||";\break
	\skip 2; \skip 2*12;\skip 1.; \bar "||";\break
	\skip 2; \skip 2*4; \skip 1.; \bar "||";
	\skip 2; \skip 2*4; \skip 1.; \bar "||";\break
	\skip 2; \skip 2*12;\skip 1.; \bar "||";
}

$upper_staff = \context Staff = upper {
	\clef "treble";
	\context Staff <
		\global
		{ \voiceone \sop }
		{ \voicetwo \alt }
	>
}

$lower_staff = \context Staff = lower {
	\clef "bass";
	\context Staff <
		\global
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
	        indent = 0.0\mm;
		linewidth= 140.0\mm;
		gourlay_maxmeasures = 6.0;
		\translator{
			\StaffContext
			\remove "Time_signature_engraver";
		}
	}
}
