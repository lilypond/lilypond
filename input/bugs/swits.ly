\score{
  \context PianoStaff <
    \context Staff=one \notes{ s2 }
    \context Staff=two \notes\relative c{
      \time 4/8;
      \clef "bass";
       [c8( e \stemup 	\translator Staff=one \stemup 
      g )c]
    }
  >
  \paper {
    linewidth=-1.0;
  }
}
)
