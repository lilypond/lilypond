
\header {

    texidoc = "Beam positions may be set by hand by overriding
    positions.  No processing (quanting, damping) whatsoever is done "

}

\score {
  \context Staff = up {
   \notes\relative c'{
    \property Voice.Beam \override #'auto-knee-gap = ##f
    \property Voice.Beam \override #'positions = #'(-10 . 10)
       \stemUp
  [c16 c'' a f]

   }
  }
  \paper{
    linewidth = -1
  }
}
