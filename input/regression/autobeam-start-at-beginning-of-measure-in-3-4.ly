\version "2.13.51"

\header {

  texidoc = "
3/4 beaming has special rules, that are hardcoded in the autobeam
routines.  When the beaming is changed, beams should start at the
beginning of the measure.  In this case, the measure should be beamed
in two.
"
}

\relative c' {
    \time 3/4
    \set Timing.baseMoment = #(ly:make-moment 1 8)
    \set Timing.beatStructure = #'(3 3)
    \set Timing.beamExceptions = #'()
    c c c c c c
}
