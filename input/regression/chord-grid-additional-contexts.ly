\version "2.23.10"

\header {
  texidoc = "Contexts can be created in parallel with @code{ChordGrid}
by instantiating a @code{ChordGridScore} explicitly."
}

%% TODO: add documentation when support for anticipations is implemented. --JeanAS

\new ChordGridScore <<
  \new RhythmicStaff { \improvisationOn 4. 8~ 2 2~ 8 4. }
  \new ChordGrid \chordmode { c2 g2 c2 g2 }
>>
