\header{
filename =	 "twinkle-pop.ly";
%title =	 "Ah, vous dirais-je, maman ";
description =	 "twinkle twinkle in pop-song-settings";
composer =	 "traditional";
enteredby =	 "HWN, chords by Johan Vromans";
copyright =	 "public domain";
}

\version "1.3.93";

m =\notes  \relative c'' {
	\property Staff.automaticMelismata = ##t
	\autoBeamOff
	g4  r8 \times 2/3 { g'8( f )e } r8 \grace { [d16 c b] } e4
	\emptyText
	d8.^"melisma" 	\melisma c16
	\melismaEnd
	b c d e }

textI =  \lyrics  { la4 __ la -- la  I, la dargh la dargh.  }
textII =  \lyrics  { dar -- dargh __ dargh dargh; dargh la dargh loo.  }
textIII =  \lyrics  { la -- da __ doo dah; dargh la dargh loo.  }

\score {

    \notes < \context Staff = SA \context Voice = VA { s1 }
      \context LyricVoice = "VA-1" { s1 }
      \context LyricVoice = "VA-2" { s1 }
      
      \addlyrics
	  \context Staff = SA \m
	  < \context LyricVoice = "VA-1" \textI
	    \context LyricVoice = "VA-2" \textII
	    \context LyricVoice = "VA-3" \textIII
	  >
	  
    >

  \paper {
  }

}