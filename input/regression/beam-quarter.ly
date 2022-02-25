\version "2.19.21"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "stem does not fit in beam"))
#(ly:expect-warning (G_ "beam was started here"))

\header {
  texidoc= "Quarter notes may be beamed: the beam is halted momentarily."
}

\relative {
  c''8[ c4 c8] % should warn here!
}
