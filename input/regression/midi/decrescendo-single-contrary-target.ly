\version "2.19.45"

\header {
  texidoc="When a decrescendo is followed by an explicit dynamic that
  is louder than the starting dynamic, the dynamic performer chooses a
  reasonable target dynamic.  The velocity of notes during the
  decrescendo is linearly interpolated between the starting and target
  dynamics, with the explicit dynamic taking effect at the last
  moment."
}

\score {
   { c\mf\> c c\f }
   \midi {}
}
