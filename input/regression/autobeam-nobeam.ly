\version "2.13.61"
%%  Hack -- this should be 2.15.0, unless it is backported
%%  Set to 2.13.61 in order to keep compile working

\header {

  texidoc = "
  \noBeam should terminate an autobeam, even if it's not a
  recommended place for stopping a beam.  In this example,
  the first three eighth notes should be beamed.
  "
}

\relative c'{
  c8 d e f\noBeam  g a b c
}
