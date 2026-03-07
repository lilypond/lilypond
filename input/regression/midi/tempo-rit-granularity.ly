\version "2.27.0"

\header {
  texidoc = "The playback duration of the two MIDI files should be nearly the
same in spite of the difference in rhythm."
}

#(ly:set-option 'warning-as-error #t)

%% test different slopes
tempoI = 240
tempoII = 60
tempoIII = 30

\score {
  \fixed c' {
    \tempo 4 = \tempoI
    \startGradualTempoChange \default
    \*3 c1
    \tempo 4 = \tempoII
    \startGradualTempoChange \default
    \*3 c1
    \stopGradualTempoChange 4 \tempoIII
    \tempo 4 = 72
    c'4
  }
  \midi { }
}

\score {
  \fixed c' {
    \tempo 4 = \tempoI
    \startGradualTempoChange \default
    \*3 \*32 c32
    \tempo 4 = \tempoII
    \startGradualTempoChange \default
    \*3 \*32 c32
    \stopGradualTempoChange 4 \tempoIII
    \tempo 4 = 72
    c'4
  }
  \midi { }
}
