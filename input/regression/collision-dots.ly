


\header { texidoc = "Collision resolution tries to put notes with dots
  on the right side."
}

\version "1.9.4"
    \paper { raggedright= ##t }


\score{
        \context Staff \notes <<
                \clef "bass"
                \new Voice {
                        \stemUp
			b8.  c'16  d'4 b8  c'8  d'4
                }
                \new Voice {
                        \stemDown
			b2 b2 
                }
        >>
}



