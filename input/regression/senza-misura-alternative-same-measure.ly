\version "2.27.3"

\header {
  texidoc = "This tests bar numbers for a repeated section with alternatives
which is completely contained within one measure in a senza-misura section."
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
    \*5 c8
    \caesura
    \*5 d16
  } \alternative {
    \volta 1 {
      \volta #'() \bar ""  % need a BarLine for a BarNumber
      e4.
    }
    \volta 2 {
      \measureRemainder {
        f2.
      }
    }
  }
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
