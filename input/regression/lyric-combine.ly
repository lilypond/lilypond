\version "1.3.146"
\header{
texidoc="
Lyrics can be set to a melody automatically.  Excess lyrics will be
dumped.  Lyrics will not be set over rests.  You can have melismata
either by setting a property melismaBusy, or by setting
automaticMelismas (which will set melismas during slurs and ties).  If
you want a different order than first Music, then Lyrics, you must
precook a chord of staffs/lyrics and label those.  Of course
@code{\rhythm} ignores any other rhythms in the piece.  Hyphens and
extenders do not assume anything about lyric lengths, so they continue
to work.
"


filename = 	 "twinkle-pop.ly"
xtitle = 	 "Ah, vous dirais-je, maman "
description = 	 "twinkle twinkle in pop-song-settings"
composer = 	 "traditional"
enteredby = 	 "HWN, chords by Johan Vromans"
copyright = 	 "public domain"
}



m = \notes  \relative c'' {
	\property Staff.automaticMelismata = ##t
	\autoBeamOff
	g4  r8 \times 2/3 { g'8( f )e } r8 \grace { [d16 c b] } e4
	\emptyText
	d8.^"melisma" 	\melisma c16
	\melismaEnd
	b }

noisebeat = \notes \relative c'' {g16 g g g }
noise =   { \noisebeat \noisebeat \noisebeat  \noisebeat \noisebeat \noisebeat }
textI =   \lyrics  { la2 __ la -- la __ la la la la la  }
textII =   \lyrics  { da -- da __ da -- da da da da da  }

\score {
    \notes < \context Staff = SA \m
      \context Lyrics = LA { s1 }
      \context Staff = SB { s1 }
      \context Lyrics = LB { s1 }
      \context Staff = SC \noise
      
      \addlyrics
	  \context Staff = SB \m
	  < \context Lyrics = LA \textI
	    \context Lyrics = LB \textII
	  >
	  
    >
}

