\version "1.3.148"
\header{
texidoc = "Stripped version of trip.ly.  Staffs should be of correct length."
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
