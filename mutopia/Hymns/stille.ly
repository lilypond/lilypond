\header {
title = "Stille Nacht";
subtitle = "(Silent Night)";
filename = "stille.ly";
enteredby = "Peter Chubb";
composer = "Franz X. Gruber";
poet = "Joseph Mohr";
}


\version "1.1.52";

global=\notes {
	      \time 6/4;
	      \key Bes;
}

sop=\notes \relative f' {
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
	   c'2 c4 es4.()c8 a4 |
	   bes2.~ ( <bes2-- )d2-- > r4 |
	   bes4 ( )f d f4.( )es8  c4 | 
	   bes2. ~ bes2 r4 \bar "|.";
}

alt=\notes\relative c' {
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
	   f2.~ e!2-- r4 |
	   d2 bes4 d4. c8 c4 |
	   bes2. ~ bes2 r4 \bar "|.";
}

ten=\notes\relative c' {
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
	   bes2. ~ g2-- r4 |
	   bes2 g4 a4. bes8 a4 | 
	   bes2. ~ bes2 r4 \bar "|.";
}

bass=\notes\relative c {
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
	   d2. ~ c2-- r4 |
	   f2 f4 f4. g8 a4 |
	   bes2. ~ bes2 r4 \bar "|.";
}

chords=\lyrics{
	"B\\textflat"1 ""2
	"B\\textflat maj7"1 ""2
	"F7"2. "E\\textflat dim"2. |
	"B\\textflat /D"2 "Cm7"4 "B\\textflat"2. |
	"E\\textflat"1 ""2 |
	"B\\textflat"1 ""2 |
	"E\\textflat"1 ""2 |
	"B\\textflat"1 ""2 |
	"Cm  "4 "Gm7sus4/D "4 "Cm/E\\textflat "4 "F7"2 "E\\textflat dim"4 |
	"B\\textflat /D"2. "C9"2 ""4 |
	"B\\textflat /F"2. "F7"2. |
	"B\\textflat"2 ""2 ""2
}

v1 = \lyrics {
       Stil-2 le4 Nacht,2. hei-4. li-8 ge4 Nacht,2. 
       Al-2 les4 schl\"aft,2. ein-2 sam4 wacht2.
       Nur2 das4 trau-2 te,4 hoch-4. heil8 ige4 Paar,2.
       Hold-2 er4 Knabe2 im4 lock-4. i-8 gen4 Haar2. 
       Schlaf2 in4 himm-4. li-8 scher4 Ruh'!2 - -
       Schlaf2 in4 himm-4. li-8 scher4 Ruh'!2 - -
}
v2 = \lyrics {
       Stil-2 le4 Nacht,2. hei-4. li-8 ge4 Nacht,2. 
       Hir-2 ten4 erst2. kund2 ge-4 macht2.
       Durch2 der4 En-2 gel4 Ha-4. lle-8 lu4 ja2.
       T\"ont2 es4 laut2 von4 fern2 und4 nah:2.
       "``Christ,"2 der4 Ret-4. ter,8 ist4 "da!''"2 - -
       "``Christ,"2 der4 Ret-4. ter,8 ist4 "da!''"2 - -
}

v3 = \lyrics {
       Stil-2 le4 Nacht,2. hei-4. li-8 ge4 Nacht,2. 
       Go-2 ttes4 Sohn,2. o-2 wie4 lacht2.
       Lieb'2 aus4 dei-2 nem4 g\"ott-4. li-8 chen4 Mund,2.
       Da2 uns4 schlägt2 die4 ret-2 tende4 Stund'.2. 
       Christ,2 in4 Dei-4. ner8 Ge-4 burt!2 - -
       Christ,2 in4 Dei-4. ner8 Ge-4 burt!2 - -
}

v1e = \lyrics {
	Sil-2 ent4 night,2. Ho-2 ly4 night,2.
	All2 is4 calm,2. all2 is4 bright,2.
	'Round2 yon4 Vir-2 gin4 Mo-4. ther8 and4 Child2.
	Ho-2 ly4 In-4. fant8 so4 ten-4. der8 and4 mild,2.
	Sleep2 in4 hea-4. ven-8 ly4 peace,2 - -
	Sleep2 in4 hea-4. ven-8 ly4 peace.2 - -
}
v2e = \lyrics {
	Si-2 lent4 night,2. Ho-2 ly4 night,2.
	Shep-2 herds4 quake2. at2 the4 sight,2.
	Glo-2 ies4 stream2 from4 hea-4. ven8 a-4 far,2.
	Hea-2 v'nly4 hosts2 sing4 "``A-"4. lle-8 lu-4 ia;2.
	Christ2 the4 Sa-4. viour8 is4 born,2.
	Christ2 the4 Sa-4. viour8 is4 "born.''"2.
}

v3e = \lyrics {
	Si-2 lent4 night,2. Ho-2 ly4 night,2.
	Son2 of4 God,2. love's2 pure4 light2.
	Ra-2 diant4 beams4. from8 Thy4 ho-4. ly8 face,2.
	With2 the4 dawn2 of4 sa-4. ving8 grace2.
	Je-2 sus,4 Lord,4. at8 Thy4 birth,2.
	Je-2 sus,4 Lord,4. at8 Thy4 birth.2.
}


upper= \notes {
	\context Staff = upper { 
		\clef "treble"; \global
		\context Staff <
			{ \voiceone \sop }
			{ \voicetwo \alt }
		>
	}
}

lower = \notes {
	\context Staff = lower {
		\clef "bass"; \global 
		\context Staff <
			{ \voicethree \ten }
			{ \voicefour \bass }
		>
	}
}

\score {
       \context ChoirStaff 
       <
	      { \context Lyrics = top \chords }
              \upper
	      { \context Lyrics = upper \v1 }
	      { \context Lyrics = upper \v2 }
	      { \context Lyrics = upper \v3 }
	      \lower
       >
}
