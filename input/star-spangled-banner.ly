%{
Converted from star.mup with the aid of mup2ly.py
http://www.Arkkra.com/doc/star.html
http://www.Arkkra.com/doc/star.ps
%}



\header{
	title="The Star Spangled Banner";
	subtitle="The United States National Anthem";
	poet="Text by Francis Scott Key";
	composer="J. S. Smith";
	arranger="Arranged by William J. Krauss";
	enteredby="jcn";
	copyright="public domain";
}

$staff1_voice_1 = \notes {
  a8.()fis16 
  \repeat volta 2 { d4 fis4 a4 d'2 fis'8. e'16 d'4 fis4 gis4 a2 a8 a8 
  fis'4. e'8 d'4 cis'2 b8. cis'16 d'4 d'4 a4 }
  \alternative { { fis4 d4 a8. fis16 } { fis4 d4 fis'8. fis'16 } } 
  fis'4 g'4 a'4 a'2 g'8 fis'8 e'4 fis'4
  g'4 g'2 g'4 fis'4. e'8 d'4 cis'2 b8. cis'16 d'4 fis4 gis4 a2 a4
  d'4 d'4 d'8()cis'8 b4 b4 b4 e'4 g'8 ()fis'8  e'8()d'8 
  d'4()cis'4 a8. a16 d'4.()e'8 fis'8 g'8 a'2 d'8 e'8 fis'4. g'8
  e'4 d'2 s4 
}

$staff1_voice_2 = \notes { 
  a8.()fis16 
  \repeat volta 2 { a,4 d4 e4 d4 () fis4 fis8. fis16 fis4 d4 d4 cis2
  e8 e8 a4. a8 a4 a2 a8. a16 a4 a4 a4 }
  \alternative { { fis4 d4 a8. fis16 } { fis4 d4 r4 } }
  a4 a4 d'4 d'2 a8 a8 cis'4 cis'4 cis'4 cis'2 a4 a4. a8 a4 a2
  a8. a16 d4 d4 d4 cis2 e4 fis4 e4 d4 d4 d4 dis4 g4 g8()dis8 e4 e2
  e8. e16 d4.()a8 a8 a8 a2 g8 g8 a4. a8 g4 fis2 s4 
}

$staff2_voice_1 = \notes { 
  r4 
  \repeat volta 2 { fis4 a4 a4 b2 cis'8. cis'16 b4 b4 b4 a2 cis'8 cis'8 
  d'4. cis'8 d'4 e'2 e'8. e'16 d'4 d'4 a4 }
  \alternative { { fis4 d4 r4 } { fis4 d4 r4 } }
  d4 e4 fis4 fis'2 e'8 d'8 e'4 e'4 e'4 e'2 cis'4 d'4. cis'8 d'4 e'2
  e'8. e'16 a4 a4 e4 e2 cis'4 a4 a4 a4 g4 g4 b4 b4 b4 b4 a2
  cis'8. cis'16 a4.()cis'8 d'8 d'8 d'2 d'8 d'8 d'4. d'8 cis'4
  a2 s4 
}

$staff2_voice_2 = \notes { 
  r4 
  \repeat volta 2 { d4 d4 cis4 b,2 ais,8. ais,16 b,4 b,4 e4 a,2 a8 a8 
  d4. e8 fis8 g8 a2 g8. g16 fis4 fis4 a4 }
  \alternative { { fis4 d4 r4 } { fis4 d4 r4 } } 
  d4 d4 d4 d2 d8 d8 a4 a4 a4 a2 a,4 d4. e8 fis8 g8 a2 g8. g16 
  fis4 d4 e4 a,2 a4 d4 e4 fis4 g4 g4 fis4 e4 e8()fis8  g8()gis8 a2 
  g8. g16 fis4.()a,8 d8 e8 fis2 b8 b8 a4. a8 a,4 d2 s4 
}


altOne =\lyrics {  say,4 can you | see,2 __ by8. the16 dawn's4 ear- ly light2 What8
       so8 proud-4. ly8 we4 hailed,2 At8. the16 twi-4 light's last gleam-
       ing. Whose8. broad16  }

altTwo =     \lyrics {  stripes4 and bright stars,2 through8. the16 per-4 il- ous fight,2
      O'er8 the8 ram-4. parts8 we4 watched,2 were8. so16 gal-4 lant- ly 
	_ _   % UGH UGH
	stream-4 ing
  And8. the16 rock-4 ets' red glare,2 the8 bombs8
  burst-4 ing in air,2 gave4 proof4. through8 the4 night2 that8.
  our16 flag4 was still there,2 Oh4 say, does that star- span-
  gled ban- ner yet wave,2 __ O'er8. the16 land2 __ of8 the8 free2
  and8 the8 home4. of8 the4 brave.2
}
% old code
nonRepeatText = \lyrics  \context LyricVoice = LVA {
  Oh4 __
  \context Lyrics <
	\altOne  
	\altTwo 
  >
}

%{
Be careful to have all duration within the lyrics of the same kind.

Alternatives & need to be matched more intelligently.  See
"whose broad" alt1, volta1, "streaming" alt2, *volta2*!

%}

textxx = \lyrics {  
  Oh __ \repeat "fold" 2 { }
  \alternative {
     { 
       say, can you | see, by the dawn's ear- ly light What
       so proud- ly we hailed, At the twi- light's last gleam-
       ing. Whose broad 
     }

    { 
      stripes and bright stars, through the per- il- ous fight,
      O'er the ram- parts we watched, were so gal- lant- ly
	" " " " " "  " "% _ _ _ _ % UGH UGH UGH 
	stream- ing
  And the rock- ets' red glare, the bombs
  burst- ing in air, gave proof through the night that
  our flag was still there, Oh say, does that star- span-
  gled ban- ner yet wave, __ O'er the land __ of the free
  and the home of the brave.
  }
}}


beginmatter = \notes {\time 3/4; \key d;  s4 | }
			

automaticLyrics = \notes \context GrandStaff < 
		\addlyrics
		\context Staff=staffA <
			\beginmatter
			\clef treble;
			\property Staff.automaticMelismata = ##t
			\context Voice = one \transpose c'' {\stemup \$staff1_voice_1 \bar "|.";} 
			\context Voice = two \transpose c'' {\stemdown \$staff1_voice_2 } 
		>
		\context LyricVoice \textxx
		\context Staff=staffB <
			\beginmatter
			\clef bass;
		\property Staff.noVoltaBraces = ##t
			\context Voice = three {\stemup \$staff2_voice_1 \bar "|."; } 
			\context Voice = four {\stemdown \$staff2_voice_2 } 
		>
	>

handLyrics = \notes \context GrandStaff < 
		\context Staff=staffA <
			\beginmatter
			 \transpose c'' {\voiceone \$staff1_voice_1 \bar "|.";} 
			 \transpose c'' {\voicetwo \$staff1_voice_2 } 
		>
		\nonRepeatText
		\context Staff=staffB <
			\beginmatter
			\clef bass;

			\property Staff.noVoltaBraces = "1"
			{\voiceone \$staff2_voice_1 \bar "|."; } 
			{\voicetwo \$staff2_voice_2 } 
		>
	>


\include "paper16.ly";

\score{ 
	\automaticLyrics
	\paper{
		\paper_sixteen
		textheight = 230.\mm;
		linewidth= 180.\mm;
		\translator {
			\GrandStaffContext
			\accepts "Lyrics";
		}
		\translator {
			\LyricsContext
			\consists "Bar_engraver";
		}
	}
	\midi {}
}


\version "1.3.4";
