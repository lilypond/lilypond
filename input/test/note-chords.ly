\header {

    texidoc= "Chord names don't attempt to find inversions and
bass notes."  }

\version "1.7.16"

scheme = \notes {
  <<c' e' g'>>1
  <<e' g' c''>>
  <<e e' g' c''>>
}

\score {
  <
    \context ChordNames \scheme
    \context Staff \scheme
  >
  \paper { raggedright = ##t}
}
%% new-chords-done %%
