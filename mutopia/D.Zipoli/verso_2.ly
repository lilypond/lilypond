\header{
filename="verso_2.ly";
enteredby = "Peter Chubb";
arranger = "Peter Chubb";
composer = "Domenico Zipoli";
date = "c1700";
title = "Verso II";
}

%{
	This is an organ piece that I've arranged for Recorder trio.
	The third part can be played either on a second treble 
	recorder, or on a tenor.

	Copright 1998 Peter Chubb.
	This work may be used and modified freely 
	under the Gnu Public Licence.
%}
	
\version "1.3.93";

$voice_one = \notes \relative c' {
	a'2 bes4. [c16 bes] | a4 d ~ d c ~ | c b ~ [b8 a] a4 ~|
	[a8 gis16 fis16] gis4 a4^"'" e'4 | f2 ~ [f8 g16 f] e4 ~| [e8 f16 e] [d8 c] d2 |
	c2 bes4 ~ [bes8 c16 bes] | a4~[a8 bes16 a] g4~ [g8 a16 g] f4 ~ [ f8 g16 f] e4 a4~|
	[a8 g16 fis] g4 ~ g f | e2 ~ e4 g4 ~ | g [fis8 e] fis2 \bar "|.";
}

$voice_two = \notes \relative c' {
	[d8 e f d ] g4 c, | f d e2 | f4 ~ [f8 g16 f] e4 a|
	d,4. d8 c4 a'4 | r4 a4 bes4 ~ [bes8 c16 bes] | a2 ~ [a8 bes16 a] g4 ~|
	[ g8 a16 g ] f4 ~ [f8 g16 f] e4 | f2 ~ f4 e4 ~| e4 d4~ d4 cis4 |
	d2 cis4 d4~ | d4 [cis8 b] cis2 | d1 \bar "|.";
}

$voice_three = \notes \relative c'
 {
	r1 | r2 [a8 b c a] | d4 g, c4. [d16 c] |
	b4. b8 [a b c a ] | [d, e f d ] g4 c,4 | f2 bes4 ~ [bes8 c16 bes]|
	a4 ~ [a8 bes16 a] g4 c, | [f8 g a f ] [c d e cis] | [d e f d] a2 |
	bes4. [c16 bes] a4 d | a1 | d1 \bar"|.";
}

global=\notes {
	\time 2/2;
	\keysignature  c;
	\property Staff.timeSignatureStyle = "C"
}


recorder= {
%	For three recorders.
%
	\context StaffGroup
	<
		\context Staff = descant {
			\property Staff.Instrument = "Descant"
			\clef "G^8";
			\notes \transpose bes' {\global \$voice_one }
		}

		\context Staff = treble {
		      \property Staff.Instrument = "Treble"
			\clef "G";
			\notes \transpose bes' {\global  \$voice_two }
		}

		\context Staff = lower {
		      \property Staff.Instrument = "Tenor or Treble II"
			\clef "G";
			\notes \transpose bes'' {\global \$voice_three }
		}
	>
}

%
% For keyboard (Organ)
% Not quite the same as the original.
% TODO:  add inner part;
%	 Try to make parts cross staves as apropriate.
%
organ={
	\context GrandStaff
	<
		\context Staff = treble {
		      \clef "G";
		      \global
		      \context Staff <
		        { \voiceOne \$voice_one }
		        { \voiceTwo \$voice_two }
		      >
		}
		\context Staff = bass {
		      \clef "F"; \global \$voice_three
		}
	>
}

\score{
	\recorder

	\paper{}
}

