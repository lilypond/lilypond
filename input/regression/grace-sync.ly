#(ly:set-option 'old-relative)
\version "1.9.8"
\header  {
  texidoc = "Grace notes in different voices/staves are synchronized."
}

\score  {\notes\relative c'' << \context Staff  { c2
	 \grace  c8
  c4 c4 }
		\new Staff { c2 \clef bass
 \grace {  dis8[ ( d8] \key es\major  }

    c4) c4 }
		\new Staff { c2 c4 c4 \bar "|." }
		>>
		\paper { raggedright = ##t}
 } 

