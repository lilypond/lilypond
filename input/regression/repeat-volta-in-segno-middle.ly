\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This tests a @code{\\repeat volta} inside and in the middle
of a @code{\\repeat segno}.  The music unfolds to ABCBDE ABCBDE."
}

piece = \new Voice \fixed c' {
  \repeat segno 2 {
    s1_"A"
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
