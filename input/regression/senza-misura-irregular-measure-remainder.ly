\version "2.27.3"

\header {
  texidoc = "@code{\\measureRemainder} can be used to finish an irregular
measure when @code{\\senzaMisura} is in effect."
}

#(ly:set-option 'warning-as-error #t)

\new Score \with {
  \verboseBarNumbers
  \senzaMisuraTimeSignatureX
} \fixed c' {
  \senzaMisura
  c\breve \measureRemainder { d8. d4 }  % continue senza misura
  e2 \measureRemainder { e8.... }       % start a meter
  \time 4/4
  c1
}
