\version "1.3.146"

\header {
texidoc="You can get ugly slurs, if you want.
"
}

baseWalk = \notes \relative c {
  d,8( a' d f a d f d a f d )a
}

\score {
  \notes \context PianoStaff <
    \time 6/4
    \context Staff=up { s1 * 6/4 }
    \context Staff=down <
      \clef bass
      \autochange Staff \context Voice \baseWalk
    >
  >
  \paper {
    linewidth = -1.
    \translator {
      \VoiceContext
      Slur \override #'beautiful = #5.0
      Slur \override #'direction = #1
      Stem \override #'direction = #-1
      autoBeamSettings \override #'(end * * * *)
        = #(make-moment 1 2)
    }
    \translator {
      \PianoStaffContext
      VerticalAlignment \override #'threshold = #'(5 . 5)
    }
  }
}

