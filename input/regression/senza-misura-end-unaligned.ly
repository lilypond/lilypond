\version "2.27.3"

\header {
  texidoc = "A senza-misura section does not have to end on a measure
boundary."
}

#(ly:set-option 'warning-as-error #t)

\new Score \with {
  \verboseBarNumbers
  \senzaMisuraTimeSignatureX
  caesuraType = #'((underlying-bar-line . ";"))
} \fixed c' {
  \senzaMisura
  \*13 c8
  \section
  \time 4/4
  \premeasure c4 |
  c1
}
