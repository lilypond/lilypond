

\header { texidoc = "collision resolution tries to put notes with dots
  on the right side."
}

\version "1.5.68"

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


