\header {
texidoc = "Tied accidentaled notes (which cause reminder accidentals) don't wreak havoc in
the spacing when unbroken."
}

\version "1.7.6"
\include "deutsch.ly"

rechts = \notes \relative c' {
  \clef treble
  \time 3/4
  \context Voice {
    c8 h2  <<g h des f>>8 ~ |
    <<g h des f>>8
  }
}

Rechts = \context Staff \notes {
  rechts
}

\score {
  \rechts

  \paper {
    linewidth = -3.3 \cm
  }
}
%% new-chords-done %%