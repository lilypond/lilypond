\version "2.1.22"

\header {
texidoc = "Stanza numbers are put left of their lyric."
}

\score {
<<
    \notes { r4 r4 c4  c4 }
    \context Lyrics
    \lyrics {
	\skip 2
	\set stanza = "1."
	Foo8 Bar8
    }
>>

\paper { raggedright = ##t } 
} 


