
\header {
texidoc = "Tied accidentaled notes (which cause reminder accidentals) don't wreak havoc in
the spacing when unbroken."
}

\version "1.9.8"

rechts = \notes \relative c' {
  \clef treble
  \time 3/4
    c8 b2  <g b des f>8 ~ |
    <g b des f>8
}

\score {
  \rechts

  \paper {
    raggedright = ##t
  }
}
