\header {
  texidoc="beam-stem attachment test"
}
	
\score {
  \context RhythmicStaff \notes {
    \stemUp [c8 c]
    \stemDown [c8 c]
    \stemUp [c8. c16]
  }
  \paper {
    linewidth = -1.0
    magnification = 64
  }
}