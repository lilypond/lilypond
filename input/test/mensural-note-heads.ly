
\version "2.8.0"

\header {
    texidoc ="@cindex Ancient Mensural Note Heads
Mensural notes may also have note heads. "
}

\score {
	 {
	\relative c'' {
		\override NoteHead  #'style = #'mensural
		c\maxima*1/8
		c\longa*1/4 c\breve*1/2 c1 c2 c4 c8 
		}
	}
	\layout{ragged-right=##t}
}

