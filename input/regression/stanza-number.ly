\version "2.1.30"

\header {
texidoc = "Stanza numbers are put left of their lyric. Theyr are aligned in a column."
}

\score {
<<
    \context Voice = "A" \notes \relative c'' { r4 r4 c4  c4 }
    \lyricsto A \new Lyrics \lyrics  {
	\skip 2
	\set stanza = "1."
	Foo8 
    }
    \lyricsto A \new Lyrics \lyrics {
	\skip 2
	\set stanza = "2."
	FFFooooo8
    }
>>

\paper { raggedright = ##t } 
}


