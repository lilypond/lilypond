\version "2.27.5"

\header {
  texidoc = "When @code{\\once \\override} is moved due before grace
notes in other tracks, its undo operation is shifted to the originally
scheduled point of time.  In the displayed staves, only one element at the
front should appear colored red: though the overrides all happen at the start
of grace time, they are unwound as required."
}


<<
  \new Staff { \once \set Staff.instrumentName = "I"
               \once \override Staff.TimeSignature.color = #red
               \time 4/4 c'4 4 4 4 }
  \new Staff \with { instrumentName = "II" }
             { \once \override NoteHead.color = #red c'4 4 4 4 }
  \new Staff { \once \set Staff.instrumentName = "III"
               \once \override NoteHead.color = #red \grace { c'8 } 4 4 4 4 }
  \new Staff { \once \set Staff.instrumentName = "IV"
               \once \override NoteHead.color = #red \grace { c'8 8 } 4 4 4 4 }
  \new Staff { \once \set Staff.instrumentName = "V"
               \once \override NoteHead.color = #red \grace { c'8 8 8 } 4 4 4 4 }
>>
