\header {

    texidoc = "Volta brackets can be placed over chord names. This
requires adding an engraver to @code{ChordNames}, and setting
@code{voltaOnThisStaff} correctly."

}

\version "2.2.3"
\score {
    <<
  \new ChordNames \with {
      voltaOnThisStaff = ##t
  } \chords {
     c1 c
  }
  \new Staff \with {
      voltaOnThisStaff = ##f
  } \notes {
   \repeat volta 2 { c'1 } \alternative { c' }
  }
>>
}
