\version "2.19.21"

\header {
  texidoc = "
Beamlets can be set to point in the direction of the beat to which they
belong.  The first beam avoids sticking out flags (the default);
the second beam strictly follows the beat.
"
}


\relative {
  \time 6/8
  a'8. a16 a a
  \set strictBeatBeaming = ##t
  a8. a16 a a
}
