
\header {
texidoc = "Tied accidentaled notes (which cause reminder accidentals) do not 
wreak havoc in the spacing when unbroken."
}

\version "2.3.16"

rechts =  \relative c' {
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
