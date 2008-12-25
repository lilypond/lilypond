\version "2.12.0"

\header {

  texidoc = "Hyphens are printed at the beginning of the line only when
they go past the first note. "

}


\layout   {
  indent = 0.0 \cm
  line-width = 3.4 \cm

  \context {
    \Staff \remove "Time_signature_engraver"
  }
}



<<
  \new Voice \relative c'' {
    \time 1/4 c16[ c c  c]
    \time 1/4
    c16[ c c c]
    \time 1/4
    r c16[ c c]
  } \addlyrics {
    bla16 -- bla -- bla -- bla --
    bla -- bla -- bla -- bla8 --
    bla16 -- bla -- bla 
  }
>>

  
