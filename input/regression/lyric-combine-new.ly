\version "2.1.21"
\header {

    texidoc = "With the @code{\\lyricsto} mechanism, individual lyric
    lines can be associated with one melody line. For each lyric line,
    can be tuned whether to follow melismata or not."
    
}

\score {
<<
    \notes \context Voice = "bla" \relative c'' {
	\autoBeamOff
	c2( d4) e8[ c b c] f4
    }
    \lyricsto "bla" \lyrics \new Lyrics { bla ab blob blob }
    \lyricsto "bla" \lyrics \new Lyrics {
	bla 

	\property Lyrics . ignoreMelismata = ##t
	
	blob

	%% note: effect of ignoreMelismata delayed one time step.
	\property Lyrics . ignoreMelismata \unset
	blob
	
	blob
    }
    
    \lyricsto "bla" \lyrics \new Lyrics { nes ted lyrics voice with more words than no tes } >>

    }
