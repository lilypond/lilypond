\version "2.3.22"
\header {
texidoc = "A clef can be folded below notes in a different staff, if
this does not disrupt the flow of the notes."
}

\score {  \relative c'' <<
\new Staff  { c4  c16[ c c  c] c4 c4 }
	\new Staff { \clef bass c,2 \clef treble  c'2 }
	>>

	\layout { raggedright = ##t}
	}

