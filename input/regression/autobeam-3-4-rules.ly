\version "2.19.40"

\header {

  doctitle = "Special beaming rules for 3/4 time"

  texidoc = "
Beaming in 3/4 time has special treatment.  By default
six eighth notes are beamed in one. Beams that would imply 6/8 time
may be avoided with @code{beamHalfMeasure = ##f}.  When the beaming
is changed, beams should start at the beginning of the measure.
"

}

\relative c' {
  \time 3/4
  \set Timing.beamHalfMeasure = ##f
  r4.^\markup "Prevent beams that imply 6/8 time" c8 c c |
  c8 c r c c r |
  r8_"but these beams are okay" c c c c c | c c c r r4 \bar "||"

  \set Timing.beamHalfMeasure = ##t
  r4.^\markup "Or allow them" c8 c c c c r c c r
  \break

  \unset Timing.beamExceptions
  r8^\markup "Beam to the beat" c c c c c
  c c c r r4 \bar "||"

  \set Timing.baseMoment = #(ly:make-moment 1/8)
  \set Timing.beatStructure = 3,3
  r8^\markup "Override to beam groups of 3 eighth notes" c c c c c
  r4. c8 c c c c c r4.
}
