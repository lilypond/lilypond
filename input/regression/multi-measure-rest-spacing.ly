\header {

  texidoc = "By setting texts starting with a multi-measure rest, an 
extra spacing column is created. This should not cause problems."
}

  \layout {
    ragged-right = ##t
  }

\version "2.16.0"


<<
  \set Score.skipBars = ##t
  \new Staff \new Voice { 
    <<  { R1*40 }  { s1*0_"bla" }>> 
  }
>>

 
