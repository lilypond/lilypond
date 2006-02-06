\header {
    texidoc = "Volta brackets can be placed over chord names. Just set
the @code{voltaOnThisStaff} property to true for the @code{ChordNames} context and to false for the topmost ordinary @code{Staff} context."
}

\version "2.7.32"
\score { <<
  \new ChordNames \with {
    voltaOnThisStaff = ##t
  } \chordmode {
    c1 c
  }
  \new Staff \with {
    voltaOnThisStaff = ##f
  }
  {
    \repeat volta 2 { c'1 } \alternative { c' }
  }
>> }
