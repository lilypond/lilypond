\version "2.25.0"

\header {
  
  texidoc = "Whole note tremolos that begin on the third line of
the staff should have a direction similar to their beamed
counterparts."
}

\relative {
  \time 3/2
  \repeat "tremolo" 16 { b'32 a }
  \repeat "tremolo" 8 { b32 a }
  \repeat "tremolo" 16 { b32 c }
  \repeat "tremolo" 8 { b32 c }
}
