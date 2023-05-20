\version "2.25.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "Simultaneous, valid bar checks at the end of volta-style
repeat alternatives are not susceptible to false warnings.  This test
should run without warnings."
}

music = \fixed c' {
  \repeat volta 2 {
    c4
    \alternative {
      \volta 1 { d2. | }
      \volta 2 { e2. | }
    }
  }
}

<<
  \new Staff \music
  \new Staff \music
>>
