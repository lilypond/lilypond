\version "1.5.68"
\header {
  texidoc="Beam-stem attachment test.  Helper file for ps/dvips problems."
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