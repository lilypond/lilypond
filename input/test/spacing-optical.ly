
\header {
texidoc  = "Stem directions and head positions are taken into account for spacing "

}

\version "2.10.0"

\paper {
  ragged-right = ##t
  indent = #0.0
}

music = {
   c'4 e''4 e'4 b'4 |
   \stemDown
   b'8[ e'' a' e'']
   \stemNeutral
   e'8[ e'8 e'8 e'8]  
}



\score
{
  \music
  \layout {
    \context {
      \Staff
      \override NoteSpacing #'stem-spacing-correction = #0.6
    }
  }
}

\score
{
  \music
  \layout {
    \context {
      \Staff
      \override NoteSpacing #'stem-spacing-correction = #0.0
      \override NoteSpacing #'same-direction-correction = #0.0
      \override StaffSpacing #'stem-spacing-correction = #0.0
    }
  }
}
