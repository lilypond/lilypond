\header{
texidoc="stripped version of trip.ly.  upper staff is too short, lower too long"
}
\score{
  \context PianoStaff \notes <
    \context Staff = treble {
       r1
       r1
       \bar "|."
    }
    \context Staff = bass {
      r1
      \context Staff {
	\grace { c16 } c1
      }
    }
  > 
  \paper { }
}
