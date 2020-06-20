
\version "2.19.21"

\header {
  texidoc = "Outside staff callbacks that no longer apply to grobs
because they are outside the X boundary of a slur should terminate
early.  The example below should generate no warnings about Bezier
curves and there should be no change in StrokeFinger position between
the first and second examples.
"
}

\relative {
  \set strokeFingerOrientations = #'(up)
  \override StrokeFinger.avoid-slur = #'outside
  \autoBeamOff
  <a'\rightHandFinger #2 >16 b
  <a\rightHandFinger #2 >16( b)
}
