\version "1.7.18"
\header {
  texidoc="@cindex Beam Stem Test
Beam-stem attachment test.  Helper file for ps/dvips problems."
}
	
\score {
  \context RhythmicStaff \notes {
    \stemUp  c8-[ c]
    \stemDown  c8-[ c]
    \stemUp  c8.-[ c16]
  }
  \paper {
    raggedright = ##t
    magnification = 64
  }
}%% new-chords-done %%
