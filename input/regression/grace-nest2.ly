\version "1.5.68"
\header {
texidoc = "grace code should not be confused by nested sequential musics, containing grace notes; practically speaking, this means that the end-bar and measure bar coincide in this example." 

}

\score { \notes \context Voice {
	 { \grace b'8 c''2 }
       \grace b'16 c''4 c''4 \bar "|."
    }}
