\version "2.3.8"
\header {
    texidoc = "Jazz chord names can also be printed without notes."
}
\paper{
    \context{
	\ChordNames

	\override BarLine #'bar-size = #4

	
	\consists Bar_engraver
	\consists "Volta_engraver"
    }
    raggedright = ##t
}


\context ChordNames \chords {
    \repeat volta 2 {
	f1:maj f:7 bes:7
	c:maj  es
    }
}

