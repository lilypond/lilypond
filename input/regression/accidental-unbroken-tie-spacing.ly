\header {
texidoc = "Tied accidentaled notes (which cause reminder accidentals) don't wreak havoc in
the spacing when unbroken."
}

\version "1.7.18"
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
    raggedright = ##t
  }
}
%% new-chords-done %%