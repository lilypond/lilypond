\version "2.25.0"

\header {
 texidoc = "This doesn't produce a warning regarding weird stem
size." 
}

#(ly:set-option 'warning-as-error #t)

{
  \override Beam.positions = #'(1 . 1.5)
  \repeat tremolo 32 { g64 a'' }
}
