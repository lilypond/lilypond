\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This tests a @code{\\repeat volta} inside and at the start
of a @code{\\repeat segno}.  The music unfolds to A@tie{}BCBDE BCBDE."
}

piece = \new Voice \fixed c' {
  s1_"A"
  \repeat segno 2 {
    \repeat volta 2 {
      s1_"B"
      \alternative {
        s1_"C"
        s1_"D"
      }
    }
    s1_"E"
  }
}

\new Score { \piece }
\new Score { \unfoldRepeats \piece }
