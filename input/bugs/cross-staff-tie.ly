\header {

    texidoc = "Tie code is not prepared  for staff changes."
}


\version "1.5.68"
\score { \notes
  \context PianoStaff <
	  \context Staff = up { \clef G
      c'2 ~ \translator Staff=down c'
    }
    \context Staff = down { \clef F
      s1
    }
  >
  \paper { }
}



