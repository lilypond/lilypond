\header{
filename =	 "twinkle-pop.ly";
%title =	 "Ah, vous dirais-je, maman ";
description =	 "twinkle twinkle in pop-song-settings";
composer =	 "traditional";
enteredby =	 "HWN, chords by Johan Vromans";
copyright =	 "public domain";
}

\version "1.2.0";

m =\notes  \relative c'' {
	\property Staff.automaticMelismata = "1"
	\autoBeamOff
	g4  r8 \times 2/3 { g'8( f )e } r8 \grace { [d16 c b] } e4
	\property Staff.textEmptyDimension = "1"
	d8.^"melisma" 	\melisma c16
	\melismaEnd
	b }

noisebeat =\notes \relative c'' {g16 g g g }
noise =  { \noisebeat \noisebeat \noisebeat  \noisebeat \noisebeat \noisebeat }
textI =  \lyrics  { la2 __ la -- la __ la la la la la  }
textII =  \lyrics  { da -- da __ da -- da da da da da  }

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

