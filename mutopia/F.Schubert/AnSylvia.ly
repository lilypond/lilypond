\header {
filename = "AnSylvia.ly";
enteredby = "Peter Chubb";
composer = "Franz Schubert";
poet =	  "Original text by William Shakespeare\\\\" +
	  "German text by Eduard von Bauernfeld";
opus = "D. 891";
date = "1826";
title = "An Sylvia";
}


\version "1.1.66";

\include "paper16.ly";
\include "paper13.ly";

pianoRH=\notes \relative c'  {
		 \clef "G";
%1
		 <[b8-.(\pp d-. g-.> <b-. d-. g-.> <b-. d-. g-.> <)b-. d-. g-.>]
		 <[b8-. d-. g-.(> <b-. d-. g-.> <b-. d-. g-.> <)b-. d-. g-.>]
		 |
%2
		 <[c d g_"{\it simile}" \< > <c d g> <c d fis> <c d fis>]
		 <[c d fis> < \! c d fis > <c d fis> <c d fis>]
%3
		 |
		 <[d f gis \> > <d f gis> <d f gis> < \! d f gis >]
		 <[c e a> <c e a> <e a c> <e a c>] 
		 |
%4
		 <[d a' b> <d a' b> <d g! b> <d g b>] 
		 <[d g a> <d g a> <d fis a> <d fis a>] |
%5
		 \repeat semi 2 {
		 <[b_. d g( \pp > <b_. d g> <b_. d g> <)b_. d g>]
		 <[b_. d g(> <b_. d g> <b_. d g> <)b_. d g>]
		 |
%6
		 <[c d g> <c d g> <c d fis> <c d fis>]
		 <[c d fis> <c d fis> < a c dis fis> <a c dis fis>]
		 |
%7
		 <[g c e g> <g c e g> <g c e g> <g c e g >] 
		 <[g c e g> <g c e g>  <g c e> <g c e>]
		 |
%8
		 <[g c e> <g c e> <g b d> <g b d>]
		 <[g b d> <g b d> <g b d> <g b d>]
		 |
%9
		 <[fis a d> <fis a d> <fis a d> <fis a d>]
		 <[g b d> <g b d> <g b d> <g b d>]
		 |
%10
		 [< g a c d \> > <g a c d> <fis a c d> <\! fis a c d >]
		 <[g b d> <g b d><g b d><g b d>]
		 |
%11
		<[g a c d> <g a c d> <fis a c d> <fis a c d>]
		<[g b d> <g b d>] <[b d g> <b d g>]
		|
%12
		[<d fis b> <d fis b><d fis b><d fis b>]
		[<d fis b> <d fis b><d fis b><d fis b>]
		|
%13
		<[e fis b> <e fis b> < e fis ais> <e fis ais>]
		<[e fis ais> <e fis ais> <d fis b> <d fis b>]
		|
%14
		<[cis g' a!> <cis g' a> <cis a' g> <cis a' g>]
		<[cis g' a> <cis g' a> < e g a> <e g a>]
		|
%15
		<[e g a> <e g a> <d fis a> <d fis a>]
		<[d fis a> <d fis a> <d fis a> <d fis a>]
		|
%16
		<[d fis a> <d fis a> <d fis a> <d fis a>]
		<[d fis a \< > <d fis a> <d fis a> < \! d fis a >]
		|
%17
		<[d a' c \> > <d a' c><d a' c>< \! d a' c  >]
		<[d a' c> <d a' c><d a' c><d a' c>]
		|
%18
		<[d g a c \> > <d g a c> <d g a c> <d g a c>]
		<[d fis a c> <\! d fis a c \pp > <d fis a c> <d fis a c>]
		|
%19
		<[d g b> <d g b> <d g b> <d g b>]
		<[d g b> <d g b> <d g b> <s g b>]
		|
%20
		<[e g c> <e g c> <e gis c> <e gis c>]
		<[e a c> <e a c> <e b' c> < e b' c>]
		|
%21
		<[e c'> <e c'> <e a c> <e a c>]
		<[e a c> <e a c> <e a c> <e a c>]
		|
%22
		<[d a' c> <d a' c> <d a'> <d a'>]
		<[d a' d> <d a' d> <d a' c> <d a' c>]
		|
%23
		<[b d b'> <b d b'> <b d a'> <b d a'>]
		<[b d g \< > <b d g> <b d fis> < \! b d fis  >]
		|
%24
		<[b d e \> > <b d e> <gis b e> < \! gis b e  >]
		<[a c e> <a c e> <e' a> <e a>]
		|
%25
		<[d a' c> <d a' c> <d a' c> <d a' c>]
		<[d a'> <d a' \> > <c d fis> < \! c d fis  \p >]
		|
%26
		<[b d g> <b d g> <b d g> <b d g>]
		<[b d g> <b d g> <b d g> <b d g>]
		|
%27
		<[c d g > <c d g > <c d fis  > <c d fis >]
		<[c d fis \< > <c d fis >< \! c d fis  ><c d fis >]
		|
%28
		<[d f gis \> > <d f gis> < \! d f gis  > <d f gis>]
		<[c e a> <c e a> <e a c> <e a c>]
%29
		<[d a' b> <d a' b> <d g! b> <d g b>]
		<[d g a> <d g a> < d fis! a> <d fis a>]
		}
%30
		<b2 d2 g2> r2
		\bar "|.";
}

pianoLH=\notes \relative c  {
		 \clef "F";
		 g4-. d-. r [g8.( g16] | )a4-. d,-. r [a'8.( a16] |
%3
		)b4-. [d,8.( d16] )c'4-. [d,8. d16] |
%4
		d'4-. d,-. d'-. [d,8.( d16 ] 
		\repeat semi 2 {
%5
		)g4-. d_. r4 [g8.( g16] |
%6
		)a4-. d,-. r4 [a'8.( a16] |
%7
		)c4-. c,-. r [e8.( e16] |
%8
		)g4-. g,-. r4 [b'8.( b16] | 
%9
		)d4-. c-. b-. g-. |
%10
		d2(-> )g4 r4 \clef "G"; |
%11
		d'''2->( )b4 r4 \clef "F"; |
%12
		b,,4-. fis-. r [b8.( b16] |
%13
		)cis4-. fis,4-. r4 [d'8.( d16] |
%14
		)e4-. g,4-. r [cis8.( cis16] |
%15
		)d4-. d,4-. r4 [d'8.( d16] |
%16
		)fis4-. d-. a-. d-.  |
%17
		fis,4.( a8 )d4 r4 \clef "G"; |
%18
		e''2->( )d4 r4 \clef "F"; |
%19
		g,,,4-. b-. r [g8.( g16 ] |
%20
		)c4-. e-. r [gis,8.( gis16] |
%21
		)a4-. c-. r [g!8.( g16 ] |
%22
		)fis4-. d'-. r [fis,,8.( fis16] |
%23
		)g4-. g'-. r [b,8.( b16] |
%24
		)c4-. c'-. r [c,8.( c16] |
%25
		)d4-. d'-. r [d,8.( d16] |
%26
		)g4-. d-. r [g8.( g16] |
%27
		)a4-. d,-. r [a'8.( a16] |
%28
		)b4 [d,8.( d16] )c'4-. [d,8. d16] |
%29
		d'4-. d,-. d'-. [d,8.( d16 ]
		}
%30
		)g2 r2 \bar "|.";

}

tune=\notes  \relative c''  {
	       % Put dynamics over the stave.
	       \property Voice.dynamicdir = "1"

	       r1 | r1 |r1 | r1 |
		\repeat semi 2 {
%5
		b2. g4 |
%6
		g( )fis \context Staff < {\voiceone r2}{\voicetwo r4 a4}> |
%7
		g2. e4 |
%8
		e4( )d r4 \< g |
%9
		fis a g \! b |
%10
		d2 b4 r4 |
%11
		r1 |
%12
		d2. b4 |
%13
		b4( )ais r [cis8()b] |
%14
		a!2. e4 |
%15
		g4()fis r4 \< d4 |
%16
		a' fis c' \! a | 
%17
		e'4.( \> )c8 \! a4 r |
%18
		r1 |
%19
		d2. \context Staff < {\voiceone b8( )g8} {\voicetwo [b8 g8]} > |
%20
		\property Voice.slurDash = 2
		fis4( )e		\property Voice.slurDash = ""
		 \context Staff < {\voiceone r2}{\voicetwo r4 e4} > |
%21
		c'2. [b16( a g )fis] |
%22
		e4( )d \context Staff <{\voiceone r2} {\voicetwo r4 d4}> |
%23
		d2 d'2 |
%24
		e2 e,2 |
%25
		fis2. \> [g8( )a] |
%26
		\! g2 r2 |
%27
		r1 | r1 | r1
		}
		r1 \bar "|.";
}

fourbars=\lyrics { ""1*4 }
verseOne=\lyrics {
		Who2. is4 | Syl- via, ""2 |
		What2. is4 | she,2  That4 |
		all our swains com- |
		mend2 her?4 " " |
		" "1 |
		Ho-4 - - ly, |
		fair,2 ""4 and |
		wise2. is4 |
		she;2 ""4 The |
		heav'ns4 such grace did |
		lend2 her4 "" | ""1 |
		That2. a-4  |
		do- red ""2 |
		She2. might4 |
		be,2 "" |
		That2 a- |
		do- red |
		she2. might4 | be.2 "" |
}

verseTwo=\lyrics {
		Is2. she4 | kind,2 ""4 as |
		she2. is4 |fair?2 ""4 For |
		beau- ty lives with | kind-2 ness4 "" | ""1 |
		To2. her4 |eyes2 ""4 doth | Love 2. re-4 |
		pair,2 ""4 To | help him of his | blind-2 ness;4 "" |
		""1 |
		And2*3/2 be-8 ing | help'd4 - "" "" | in-2. hab-8 its |
		there,2 ""4 And |
		be-2 ing | help'd in- | ha-2 -4 bits4 | there.2 ""2 |
}

verseThree=\lyrics {
		  Then2. to4 | Syl-4 via ""2 |
		  let2. us4 | sing,2 ""4 That |
		  Syl- via is ex- | cel4 - ing; "" | ""1 |
		  She2. ex-4 | cels2 ""4 each | mor - - tal |
		  thing,2 ""4 Up- | on the dull earth | dwell-2 ing4
		  "" | ""1 |
		  To2. her4 | gar- lands ""2 | let2. us4 |
		  bring,2 "" | To her | gar- lands | let2. us4 |
		  bring.2 "" |
}


%% 
%% German Words -- syllabification may be incorrect (entered by
%% someone who knows no German!)
origVerseOne=\lyrics {
		    Was2. ist4 | Sil-4 via, ""2 |
		    sag-2. et4 | an,2 ""4 
		    Da{\ss}4 | 
		    sie die wie- te | Flur2 preist?4 "" | ""1|
		    Sch\"on2.  und4 | zart2 ""4 seh'- | ich2. sie4 |
		    nah'n,2 ""4 Auf4 |
		    Him-  melsg- gunst " und" | Spur2 weist,4 "" | ""1
		    Da{\ss}2. ihr4 | al- les ""2 |
		    un-2. ter4 | tan.2 "" |
		    Da{\ss}2 ihr | al- les | un-2.  ter4 | tan2 "" |
}

origVerseTwo=\lyrics {
		    Ist2. sie4 | sch\"on2 ""4 und4 |  gut2. da-4 | zu?2 ""4
		    Reiz4| labt wie mil- de | Kind-2 heit;4 "" | ""1 |
		    Ihr-2. em4 | Aug'-2 ""4 eilt | A-2. mor4 |
		    zu,2 ""4
		    Dort4 | heilt er sein- e | Blind-2 heit,4 "" | ""1 |
		    Und2. ver-4 | weilt2 ""4 in4 | s\"u{\ss}-2. er4
		    Ruh'.2 "" |
		    Und2 ver-2 | weilt in | s\"u{\ss}-2. er4| Ruh'.2 "" |
}

origVerseThree=\lyrics {
		      Dar4 - - um | Sil- via, ""2 | t\"on',2.  o4 |
		      Sang,2 ""4
		      Der4 | hold- en Sil- via | Ehr-2 en;4 ""4 | ""1 |
		      Je2. den4 | Reiz2 ""4  be- |siegt2. sie4 | lang,2 ""4
		      Den | Er- de kann ge- | w\"ah-2 ren;4 "" | ""1 |
		      Kr\"an-2. ze4 | ihr2 ""4 und4 | Sai-2. ten4
		      |klang!2 ""
		      Kr\"an-2 ze | ihr und | Sai-2. ten-4 |klang!2 "" |
}

global=\notes {
		\key  G;
		\time 2/2;
		\property Staff.timeSignatureStyle = "C"
}

Piano=\context GrandStaff = piano {
	    \property GrandStaff.instrument="Piano"
	    < 
	     \context Staff=RH {\global \pianoRH }
 	     \context Staff=LH {\global \pianoLH }
	    >
}


EnglishWords= \lyrics<
		    {\fourbars \verseOne \fourbars}
		    {\fourbars \verseTwo \fourbars}
		    {\fourbars \verseThree \fourbars}
>

GermanWords = \lyrics<
		     {\fourbars \origVerseOne \fourbars}
		     {\fourbars \origVerseTwo \fourbars}
		     {\fourbars \origVerseThree \fourbars}

>

Vocals= <
	\context Staff = vocal < 
	       \notes {\clef "G2"; \global\tune}
%	       {\context  Lyrics = vocal \EnglishWords}
	       {\context  Lyrics = vocal \GermanWords}
	     >
>


\score {
	<
		% \transpose aes for basses (Key Eflat).
		% (bes to c'')
		% \transpose d' for original (in A.)
		% Untransposed range: d' to e''
		\notes \transpose d' <
		      \Vocals
		      \Piano
		>
	>
	\paper {
	%	\paper_thirteen
		\paper_sixteen
		linewidth = 18.0\cm;
		textheight = 26.0\cm;
		gourlay_maxmeasures=15.0;
		\translator { \HaraKiriStaffContext }
	}
}

