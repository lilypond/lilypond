\version "2.1.4"

\header {
texidoc = "Stanza numbers are put left of their lyric."
}

\score {
<<
    \notes { r4 r4 c4  c4 }
    \context LyricsVoice
    \lyrics {
	\skip 2
	\property LyricsVoice . stanza = "1."
	Foo8 Bar8
    }
>>

\paper { raggedright = ##t } 
} 


