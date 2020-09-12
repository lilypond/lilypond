\version "2.21.4"

\header {
  texidoc = "Using @code{\override Beam.damping = #+inf.0} should
  always make beams horizontal.  A threshold is implemented to avoid
  rounding errors that would cause non-horizontal beams otherwise.
  
  Here, the beam should be horizontal."
}

#(set-global-staff-size 40)

\new Voice \relative c''' {
  \override Beam.damping = #+inf.0
  \assertBeamSlope #=
  a16 c d g,
}
