


\version "1.4.8"
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



