\header {
  title = "Stille Nacht";
  subtitle = "(Silent Night)";
  filename = "stille.ly";
  enteredby = "Peter Chubb";
  composer = "Franz X. Gruber";
  poet = "Joseph Mohr";
}


\version "1.3.42";

%{
	Note:  there are other verses, but the three here seem to be
	the most commonly sung ones.
	I don't know who did the translation or the arrangement.
%}

global=\notes {
      \time 6/4;
      \key bes;
      \skip 1.*12;
      \bar "|.";
}

sop=\notes \relative f' {
	\property Voice . articulationScriptVerticalDirection = \up
%1
	f4. ( ) g8 f4 d2. |
	f4. ( ) g8 f4 d2 r4 |
	c'2 c4 a2 r4 |
	bes2 bes4 f2. |
%5
	g2 g4 bes4.( )a8 g4 |
	f4.()g8 f4 d2 r4 |
	g2 g4 bes4.( )a8 g4 |
	f4.()g8 f4 d2 r4 |
%9	   
	c'2 c4 es4. c8 a4 |
	bes2. (  < ~bes2-- )d2-- > r4 |
	bes4 ( )f d f4.( )es8  c4 | 
	bes2. ~ bes2 r4
}

alt=\notes\relative c' {
	\property Voice . articulationScriptVerticalDirection = \down
%1
	d4. es8 d4 bes2. |
	d4. es8 d4 bes2 r4 |
	es2 es4 ges2 r4 |
	f2 es4 d2. |
%5
	es2 es4 es4. f8 es4 |
	d4. es8 d4 bes2 r4 |
	es2 es4 es4. f8 es4 |
	d4. es8 d4 c( )bes r4 |
%9
	es4( )f g a4. a8 ges4|
	f2.( )e!2-- r4 |
	d2 bes4 d4. c8 c4 |
	bes2. ~ bes2 r4
}

ten=\notes\relative c' {
	\property Voice . articulationScriptVerticalDirection = \up

	bes2 bes4 f2. |
	bes2 a4 f2 r4 |
	a2 a4 c2 r4 |
	bes2 a4 f2 ( ) bes4 |
%5
	bes2 bes4 g4. a8 bes4 |
	bes4. bes8 bes4 f2 r4 |
	bes2 bes4 g4. a8 bes4 |
	bes4. bes8 bes4 f2 r4 |
%9
	g2 bes4 c4. c8 c4 |
	bes2. ( ) g2-- r4 |
	bes2 g4 a4. bes8 a4 | 
	bes2. ~ bes2 r4
}

bass=\notes\relative c {
	\property Voice . articulationScriptVerticalDirection = \down
%1
	bes2 bes4 bes2. |
	bes2 bes4 bes2 r4 |
	f'2 f4 es2 r4 |
	d2 c4 bes2. |
%5
	es2 es4 es2 es4 |
	bes4. bes8 bes4 bes2 r4 |
	es2 es4 es4. es8 es4 |
	bes4. bes8 bes4 bes2 r4 |
%9
	c4 d es f4. f8 es4 |
	d2. ( ) c2-- r4 |
	f2 f4 f4. g8 a4 |
	bes2. ~ bes2 r4
}

harm=\chords{
	\property Score.chordInversion = 1

	bes1. |
	bes-7+ |
	f2.-7 es-dim |
	bes2/d c4-m7 bes2. |
	es1. |
	bes1. |
	es1. |
	bes1. |
	c4-m  
	g-m7.4/d      % should be Gm7sus4
	c-m/es f2-7 es4-dim |
	bes2./d c2.-9 |
	bes2./f f-7 |
	bes1.
}

v1 = \lyrics {
       Stil2 -- le4 Nacht,2. hei4. -- li8 -- ge4 Nacht,2. 
       Al2 -- les4 schl\"aft,2. ein2 -- sam4 wacht2.
       Nur2 das4 trau2 -- te,4 hoch4. -- heil8 ige4 Paar,2.
       Hold2 -- er4 Knabe2 im4 lock4. -- i8 -- gen4 Haar2. 
       Schlaf2 in4 himm4. -- li8 -- scher4 Ruh'!1 __ ""2
       Schlaf2 in4 himm4. -- li8 -- scher4 Ruh'!1 __ ""2
}

v2 = \lyrics {
       Stil2 -- le4 Nacht,2. hei4. -- li8 -- ge4 Nacht,2. 
       Hir2 -- ten4 erst2. kund2 ge4 -- macht2.
       Durch2 der4 En2 -- gel4 Ha4. -- lle8 -- lu4 ja2.
       T\"ont2 es4 laut2 von4 fern2 und4 nah:2.
       "``Christ,"2 der4 Ret4. -- ter,8 ist4 "da!''"1 __ ""2
       "``Christ,"2 der4 Ret4. -- ter,8 ist4 "da!''"1 __ ""2
}

v3 = \lyrics {
       Stil2 -- le4 Nacht,2. hei4. -- li8 -- ge4 Nacht,2. 
       Go2 -- ttes4 Sohn,2. o2 -- wie4 lacht2.
       Lieb'2 aus4 dei2 -- nem4 g\"ott4. -- li8 -- chen4 Mund,2.
       Da2 uns4 schlägt2 die4 ret2 -- tende4 Stund'.2. 
       Christ,2 in4 Dei4. -- ner8 Ge4 -- burt!1 __ ""2
       Christ,2 in4 Dei4. -- ner8 Ge4 -- burt!1 __ ""2
}

v1e = \lyrics {
	Si2 -- lent4 night,2. Ho2 -- ly4 night,2.
	All2 is4 calm,2. all2 is4 bright,2.
	'Round2 yon4 Vir2 -- gin4 Mo4. -- ther8 and4 Child2.
	Ho2 -- ly4 In4. -- fant8 so4 ten4. -- der8 and4 mild,2.
	Sleep4. __ ""8 in4 hea4. -- ven-8 ly4 peace,1 __ ""2
	Sleep4. __ ""8 in4 hea4. -- ven-8 ly4 peace.1 __ ""2
}

v2e = \lyrics {
	Si2 -- lent4 night,2. Ho2 -- ly4 night,2.
	Shep2 -- herds4 quake2. at2 the4 sight,2.
	Glo2 -- ries4 stream4. __ ""8 from4 hea4. -- ven8 a4 -- far,2.
	Hea2 -- v'nly4 hosts4. __ ""8 sing4 "``A"4. -- lle8 -- lu4 -- ia;2.
	Christ4. __ ""8 the4 Sa4. -- viour8 is4 born,1 __ ""2
	Christ4. __ ""8 the4 Sa4. -- viour8 is4 "born.''"1 __ ""2
}

v3e = \lyrics {
	Si2 -- lent4 night,2. Ho2  -- ly4 night,2.
	Son2 of4 God,2. love's2 pure4 light2.
	Ra2 -- diant4 beams4. from8 Thy4 ho2 -- ly4 face,2.
	With2 the4 dawn4. __ ""8 of4 sa2 -- ving4 grace2.
	Je2 -- sus,4 Lord,4. at8 Thy4 birth,1 __ ""2
	Je2 -- sus,4 Lord,4. at8 Thy4 birth.1 __ ""2
}


upper= \notes {
	\context Staff = upper { 
		\clef "treble"; 
		\context Staff <
			\global
			{ \voiceone \sop }
			{ \voicetwo \alt }
		>
	}
}

lower = \notes {
	\context Staff = lower {
		\clef "bass";
		\context Staff <
			\global
			{ \voicethree \ten }
			{ \voicefour \bass }
		>
	}
}

GermanWords=\context Lyrics=upper <
	\v1
	\v2
	\v3
>

EnglishWords=\context Lyrics=upper <
	\v1e
	\v2e
	\v3e
>

\score {
	\context ChoirStaff <
		\context ChordNames \harm
		\upper
%		\EnglishWords
		\GermanWords
		\lower
	>
        \paper { % fit onto single A4 page
		textheight=290.0\mm;
		indent=0.0\mm;
	}
	\midi{
	}
}

