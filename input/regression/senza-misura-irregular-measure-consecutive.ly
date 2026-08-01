\version "2.27.3"

\header {
  texidoc = "@code{\\measure} can be used to create a series of irregular
measures when @code{\\senzaMisura} is in effect."
}

#(ly:set-option 'warning-as-error #t)

\new Score \with {
  \verboseBarNumbers
  \senzaMisuraTimeSignatureX
  caesuraType = #'((underlying-bar-line . ";"))
} \fixed c' {
  \senzaMisura
  \measure c4.
  \measure { d8. d4 }
  \measure { \*5 e8 \caesura \*5 e8 e8.... }
  \time 4/4
  c1
}
