\version "1.7.18"
% delete?  I don't think it demonstrates much. -gp
% the only unusual thing here is \melisma
\header{
filename = 	 "twinkle-pop.ly"
%texidoc = 	 "twinkle twinkle in pop-song-settings"
texidoc = "DELETE (unless \melisma is really special.  In that case, rename file.) "
}



m = \notes  \relative c'' {
	\property Staff.automaticMelismata = ##t
	\autoBeamOff
	g4  r8 \times 2/3 { g'8( f e-) } r8 \grace {  d16-[ c b] } e4
	\emptyText
	d8.^"melisma" 	\melisma c16
	\melismaEnd
	b c d e }

textI =   \lyrics  { la4 __ la -- la  I, la dargh la dargh.  }
textII =   \lyrics  { dar -- dargh __ dargh dargh dargh la dargh loo.  }
textIII =   \lyrics  { la -- da __ doo dah dargh la dargh loo.  }

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
%% new-chords-done %%
