\header{
filename = 	 "twinkle-pop.ly";
xtitle = 	 "Ah, vous dirais-je, maman ";
description = 	 "twinkle twinkle in pop-song-settings";
composer = 	 "traditional";
enteredby = 	 "HWN, chords by Johan Vromans";
copyright = 	 "public domain";
}



m = \notes  \relative c'' {
	\property Staff.automaticMelismata = ##t
	\autoBeamOff
	g4  r8 \times 2/3 { g'8( f )e } r8 \grace { [d16 c b] } e4
	\emptyText
	d8.^"melisma" 	\melisma c16
	\melismaEnd
	b c d e }

textI =   \lyrics  { la4 __ la -- la  I, la dargh la dargh.  }
textII =   \lyrics  { dar -- dargh __ dargh dargh; dargh la dargh loo.  }
textIII =   \lyrics  { la -- da __ doo dah; dargh la dargh loo.  }

\score {

    \notes < \context Staff = SA \context Voice = VA { s1 }
      \context LyricsVoice = "VA-1" { s1 }
      \context LyricsVoice = "VA-2" { s1 }
      
      \addlyrics
	  \context Staff = SA \m
	  < \context LyricsVoice = "VA-1" \textI
	    \context LyricsVoice = "VA-2" \textII
	    \context LyricsVoice = "VA-3" \textIII
	  >
	  
    >

  \paper {
  }

}
