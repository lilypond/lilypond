\version "2.27.3"

\header {
  texidoc = "By default, line breaks between measures are forbidden in a
senza-misura passage, just as when there is a time signature."
}

#(ly:set-option 'warning-as-error #t)

\new Score \with {
  \verboseBarNumbers
  \senzaMisuraTimeSignatureX
} \fixed c' {
  c1 |
  \senzaMisura
  \measure { \*13 { c8[ c c c] } }
  \measure { \*13 c8 }
  \time 4/4
  c1 |
}
