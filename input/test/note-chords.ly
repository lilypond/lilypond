\header {

    texidoc= "Chord names don't attempt to find inversions and
bass notes."  }

\version "1.7.6"

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
  \paper { linewidth = -1. }
}
%% new-chords-done %%
