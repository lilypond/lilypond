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

global =  \notes {
  \time 3/4; 
  \key d \major;
  \partial 4;
}

staffBVoiceB =  \notes {
  a8.()fis16 
  \repeat volta 2 { 
    d4 fis4 a4 d'2 fis'8. e'16 d'4 fis4 gis4 a2 a8 a8 
    fis'4. e'8 d'4 cis'2 b8. cis'16 d'4 d'4 a4 
  }
  \alternative { 
    { fis4 d4 a8. fis16 } 
    { fis4 d4 fis'8. fis'16 }
  } 
  fis'4 g'4 a'4 a'2 g'8 fis'8 e'4 fis'4
  g'4 g'2 g'4 fis'4. e'8 d'4 cis'2 b8. cis'16 d'4 fis4 gis4 a2 a4
  d'4 d'4 d'8()cis'8 b4 b4 b4 e'4 g'8 ()fis'8  e'8()d'8 
  d'4()cis'4 a8. a16 d'4.()e'8 fis'8 g'8 a'2 d'8 e'8 fis'4. g'8
  e'4 d'2 s4 
}

staffBVoiceC =  \notes { 
  a8.()fis16 
  \repeat volta 2 { 
    a,4 d4 e4 d4 () fis4 fis8. fis16 fis4 d4 d4 cis2
    e8 e8 a4. a8 a4 a2 a8. a16 a4 a4 a4 
  }
  \alternative { 
    { fis4 d4 a8. fis16 } 
    { fis4 d4 r4 } 
  }
  a4 a4 d'4 d'2 a8 a8 cis'4 cis'4 cis'4 cis'2 a4 a4. a8 a4 a2
  a8. a16 d4 d4 d4 cis2 e4 fis4 e4 d4 d4 d4 dis4 g4 g8()dis8 e4 e2
  e8. e16 d4.()a8 a8 a8 a2 g8 g8 a4. a8 g4 fis2 s4 
}

staffCVoiceB =  \notes { 
  r4 
  \repeat volta 2 { 
    fis4 a4 a4 b2 cis'8. cis'16 b4 b4 b4 a2 cis'8 cis'8 
    d'4. cis'8 d'4 e'2 e'8. e'16 d'4 d'4 a4 
  }
  \alternative { 
    { fis4 d4 r4 } 
    { fis4 d4 r4 } 
  }
  d4 e4 fis4 fis'2 e'8 d'8 e'4 e'4 e'4 e'2 cis'4 d'4. cis'8 d'4 e'2
  e'8. e'16 a4 a4 e4 e2 cis'4 a4 a4 a4 g4 g4 b4 b4 b4 b4 a2
  cis'8. cis'16 a4.()cis'8 d'8 d'8 d'2 d'8 d'8 d'4. d'8 cis'4
  a2 s4 
}

staffCVoiceC =  \notes { 
  r4 
  \repeat volta 2 { 
    d4 d4 cis4 b,2 ais,8. ais,16 b,4 b,4 e4 a,2 a8 a8 
    d4. e8 fis8 g8 a2 g8. g16 fis4 fis4 a4 
  }
  \alternative { 
    { fis4 d4 r4 } 
    { fis4 d4 r4 } 
  }
  d4 d4 d4 d2 d8 d8 a4 a4 a4 a2 a,4 d4. e8 fis8 g8 a2 g8. g16 
  fis4 d4 e4 a,2 a4 d4 e4 fis4 g4 g4 fis4 e4 e8()fis8  g8()gis8 a2 
  g8. g16 fis4.()a,8 d8 e8 fis2 b8 b8 a4. a8 a,4 d2 s4 
}

text =  \lyrics {  
  Oh __ \repeat "fold" 2 { }
  \alternative {
    { 
      say, can you | see, by the dawn's ear -- ly light 
      What so proud -- ly we hailed, 
      At the twi -- light's last gleam -- ing. 
      % Ah, it seems that this context stops to exist just before
      % the :| barline is set, and doesn't see its width?
      % Ugly fix:
      Whose broad \bar "|.";
    }
    \context LyricsVoice = "one-2" 
    { 
      stripes and bright stars, through the per -- il -- ous fight,
      O'er the ram -- parts we watched, were so gal -- lant -- ly
      " " " " " "  " "% UGH UGH UGH 
      stream -- ing

      And the rock -- ets' red glare, the bombs burst -- ing in air, 
      gave proof through the night that our flag was still there, 
      Oh say, does that star -- span -- gled ban -- ner yet wave, __ 
      O'er the land __ of the free and the home of the brave.
    }
  }
}

\include "paper16.ly";

\score{ 
	\context GrandStaff \notes < 
		\addlyrics
		\context Staff=upper <
			\global
			\clef treble;
			\property Staff.automaticMelismata = ##t
			\context Voice = one \transpose c'' {
				\voiceOne
				\staffBVoiceB
				\bar "|.";
			} 
			\context Voice = two \transpose c'' {
				\voiceTwo
				\staffBVoiceC
			}
		>
		\context LyricsVoice = "one-1" \text
		\context Staff=lower <
			\global
			\clef bass;
			\property Staff.VoltaBracket = \turnOff
			\context Voice = three {
				\voiceOne
				\staffCVoiceB
			} 
			\context Voice = four {
				\voiceTwo
				\staffCVoiceC
			} 
		>
	>
	\paper{
		\paperSixteen
		textheight = 230.\mm;
		linewidth= 180.\mm;
		\translator {
			\GrandStaffContext
			\accepts "Lyrics";
		}
		\translator {
			\LyricsVoiceContext
			\consists "Bar_engraver";
		}
	}
	\midi {
		\tempo 4 = 60;
	}
}

