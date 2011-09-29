\version "2.14.0"

#(ly:set-option 'warning-as-error #f)
#(ly:expect-warning (_ "stem does not fit in beam"))
#(ly:expect-warning (_ "beam was started here"))

\header {
  texidoc= "Quarter notes may be beamed: the beam is halted momentarily."
}

\relative c'' {
  c8[ c4 c8] % should warn here!
}
