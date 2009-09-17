\version "2.12.0"

#(ly:set-option 'warning-as-error #f)

\header {
  texidoc= "Quarter notes may be beamed: the beam is halted momentarily."
}

\relative c'' {
  c8[ c4 c8] % should warn here!
}
