\header {

    texidoc = "With the newaddlyrics mechanism, individual lyric lines
    can be associated with one melody line. For each lyric line, can
    be tuned whether to follow melismata or not."
    
}

\score {
<<
    \notes \context Voice = "bla" \relative c'' {
	c2( d4) e4 ( c2 d4) e4 
    }
    \newaddlyrics "bla" \lyrics \new LyricsVoice { bla ab blob blob }
    \newaddlyrics "bla"
    \lyrics \new LyricsVoice {
	bla 

	\property LyricsVoice . ignoreMelismata = ##t
	
	blob

	%% note: effect of ignoreMelismata delayed one time step.
	\property LyricsVoice . ignoreMelismata \unset
	blob
	
	blob
    }
    >>
    }
