


\header { texidoc = "Collision resolution tries to put notes with dots
  on the right side."
}

\version "1.7.18"
    \paper { raggedright= ##t }


\score{
        \context Staff \notes <
                \clef "bass"
                \context Voice = "Tenor" {
                        \stemUp
			b8.  c'16  d'4 b8  c'8  d'4
                }
                \context Voice = "Bass" {
                        \stemDown
			b2 b2 
                }
        >
}



