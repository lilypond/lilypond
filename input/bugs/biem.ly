basloopje = \notes\relative c{
  [d,8 a' d f] [a\translator Staff=treble d f d] \translator Staff=bass 
}



lower = \type Voice=two \notes \relative c{
  <  \basloopje  >
}
\score {
    \type PianoStaff < 
\notes      \type Staff = treble { c1  }

      \type Staff = bass <
	\clef bass;
        \lower
      >
    >

  \paper {
    gourlay_maxmeasures = 4.;
    indent = 8.\mm;
    textheight = 295.\mm;

    % no slur damping
    slur_slope_damping = 100.0;
  }
  \midi {
    \tempo 4 = 54;
  }
}

