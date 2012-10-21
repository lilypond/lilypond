\version "2.16.0"
\header {

  texidoc = "Clef changes at the start of a line get much more space
than clef changes halfway the line."

}

  \layout {
    ragged-right = ##t
    \context {
      \Staff
      \remove "Time_signature_engraver"
    }
  }




<<
  \new Staff {
    c'2
    \clef bass e16 f a
    \clef treble b
  }
  \new Staff  {
    c'4 c'4 c'4 
  }
>>
