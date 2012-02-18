\version "2.15.31"

\header {

  doctitle = "Special beaming rules for 3/4 time"

  texinfo = "
Whole-measure and half-measure beaming in 3/4 time can be controlled.
The first measure should be beamed in one.  The second measure should
be beamed in three.  The third and fourth measures should be beamed
at 3/8.
  "

}

\relative c' {
    \time 3/4
    c8^\markup "Beam in one" c c c c c
    \set Timing.beamWholeMeasure = ##f
    c8^\markup "Beam in three" c c c c c
    \set Timing.beamHalfMeasure = ##t
    r4.^\markup "Beam in Two" c8 c c
    c8 c c  r4.
}
