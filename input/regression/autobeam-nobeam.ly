\version "2.16.0"

\header {

  texidoc = "
  @code{\\noBeam} should terminate an autobeam, even if it's not a
  recommended place for stopping a beam.  In this example,
  the first three eighth notes should be beamed.
  "
}

\relative c'{
  c8 d e f\noBeam  g a b c
}
