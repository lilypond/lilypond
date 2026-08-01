\version "2.27.3"

\header {
  texidoc = "This tests bar numbers for a repeated section with alternatives
which is completely contained in a senza-misura section, but with each
alternative being its own measure."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    \verboseBarNumbers
    \senzaMisuraTimeSignatureX
    caesuraType = #'((underlying-bar-line . "'"))
  }
}

testMusic = \fixed c' {
  c1 |
  \senzaMisura
  \repeat volta 2 {
    \measure {
      \*5 c8
      \caesura
      \*5 d16
    }
    \alternative {
      \volta 1 \measure {
        e4.
      }
      \volta 2 \measure {
        f2.
      }
    }
  }
  \section
  \time 4/4
  g1 |
}

\score {
  \new Staff \with { instrumentName = "default" } \testMusic
}

\new Score \with { alternativeNumberingStyle = #'numbers-with-letters } {
  \new Staff \with { instrumentName = "w/ letters" } {
    \testMusic
  }
}

\score {
  \new Staff \with { instrumentName = "unfolded" } \unfoldRepeats \testMusic
}
