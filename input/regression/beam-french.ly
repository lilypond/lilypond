\version "1.5.68"

\header {
    texidoc = "French style beaming. In french beaming, the stems do not go to the outer beams."
}

\score { \notes
\relative c'
	 {
	     \property Voice.Beam \override #'french-beaming = ##t
	     [c16 c c c]
	[c8 c16 c16 c8]
	[c8 c16 e16 g8]

    }}
