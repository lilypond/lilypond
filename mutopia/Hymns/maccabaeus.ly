\header {
filename = "maccabaeus.ly";
enteredby = "Peter Chubb";
composer = "G. F. Handel";
date = "1680";
title = "Maccabaeus";
metre = "10 11. 11 11. and refrain";
}

\version "1.0.21";

sop=\notes \relative c'' {
	b2 gis4. a8  |b2 e,2 |
	[fis8 gis] [a b] a4 gis |
	fis1 \bar "||";
	[gis8 a] [b cis] b4 b |
	e2 b |
	a4 [gis8 fis] fis4. e8 |
	e1 \bar "||";
	[gis8 fis] [gis a] gis4 gis |
	fis2 e |
	a4 gis fis e | dis1 \bar "||";
	[e8 dis] [e fis] e4 e | cis'2 ais |
	b4 [cis8 b] ais4. b8 | b1 \bar "||";
	
	b2^"{\it Refrain}" gis4. a8 | b2 e, |
	[fis8 gis] [a b] a4 gis | fis1 \bar "||";
	[gis8 a] [b cis] b4 b | e2 b |
	a4 [gis8 fis] fis4. e8 | e1 \bar "|.";
}

alt=\notes \relative c'{
	e2 e4. dis8 | e2 b |
	[dis8 e] [fis gis] fis4 e | dis1 |
	e4 e e dis | e2 e |
	dis4 e e4. b8 | b1 |
	[e8 dis] [e fis] e4 e |
	dis2 cis |
	bis4 cis cis cis | bis1 |
	[cis8 bis] [cis dis] cis4 e4|
	e2 e2 |
	dis4 gis fis fis | fis1 |
	gis2 e4. dis8 | e2 b2 |
	[dis8 e] [fis gis] fis4 e | dis1 |
	e4 e e dis | e2 e | dis4 e dis4. b8 | b1 |
}

ten=\notes \relative c' {
	gis2 b4. a8 | gis2 b |
	b4 b b b | b1 |
	b4 [b8 a] b4 a | b2 gis |
	a4 b b4. gis8 | gis1 |
	b4 b b b | a2 gis | fis4 gis a gis | gis1 |
	gis4 gis cis cis | cis2 cis |
	b4 gis cis cis | dis1 |
	e2 b4. a8 | gis2 b2 |
	b4 b b b | b1 | 
	b4 [b8 a] b4 a | b2 gis | 
	a4 [b8 cis] b4. gis8 | gis1|
}

bass=\notes \relative c {
	e2 gis4. fis8 | e2 gis |
	b4 b dis, e | b1 \bar "||";
	[e8 fis] [gis a] gis4 fis | gis2 gis |
	fis4 e b4. e8 | e1 \bar "||";
	e4 e e e | b2 cis |
	dis4 e fis cis |
	gis1 \bar "||";
	cis4 cis cis b' | ais2 fis |
	gis4 e fis fis | b1 \bar "||";
	e,2 e4. fis8 | gis2 gis,2 |
	b4 b dis e | b1 \bar "||";
	[e8 fis] [gis a] gis4 fis | gis2 gis |
	fis4 [gis8 a] b4. e,8 | e1 \bar "|.";
}

global = \notes {
	\time 2/2;
	\property Staff.timeSignatureStyle = "C"
	\key E;
}

$upper_staff = \context Staff = upper {
	\global
	\clef "treble";
	\context Staff <
	      {\voiceone \sop }
	      {\voicetwo \alt }
	>
}

$lower_staff = \context Staff = lower {
	\global
	\clef "bass";
	\context Staff <
	    {\voiceone \ten }
	    {\voicetwo \bass }
	>
}

\score {
	<
		\$upper_staff
		\$lower_staff
	>
	\paper {
	}
}
