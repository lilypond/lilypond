\header {
texidoc = "Tied accidentaled notes (which cause reminder accidentals) don't wreak havoc in
the spacing when unbroken."
}

\version "1.5.68"
\include "deutsch.ly"

rechts = \notes \relative c' {
  \clef treble
  \time 3/4
  \context Voice {
    c8 h2  <g8 h des f> ~ |
    <g8 h des f>
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
