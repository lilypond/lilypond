#(ly:set-option 'old-relative)
\version "1.9.8"
\header {
texidoc = "A clef can be folded below notes in a different staff, if
this doesn't disrupt the flow of the notes."
}

\score { \notes \relative c'' <<
\new Staff  { c4  c16[ c c  c] c4 c4 }
	\new Staff { \clef bass c,2 \clef treble  c'2 }
	>>

	\paper { raggedright = ##t}
	}

