\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This tests a @code{\\repeat segno} inside and at the end of
a @code{\\repeat volta}.  The music unfolds to ABCBD ABCBD"
}

piece = \new Voice \fixed c' {
  \repeat volta 2 {
    s1_"A"
    \repeat segno 2 {
      s1_"B"
      \alternative {
        s1_"C"
        s1_"D"
      }
    }
  }
}

\new Score { \piece }
\new Score { \unfoldRepeats \piece }
