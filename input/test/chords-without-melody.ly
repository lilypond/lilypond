\version "1.7.18"
\header {
texidoc = "Jazz chords can also be used without notes."
}

\score{
	\context ChordNames \chords{

		\repeat volta 2 {
			f1:maj f:7 bes:7
			c:maj  es
		}
	}
	\paper{
		\translator{
			\ChordNamesContext

			BarLine \override #'bar-size = #4

			
			\consists Bar_engraver
			\consists "Volta_engraver"
		}
	raggedright = ##t
	}
}



