\version "2.15.37"

\header {
  texidoc = "Lyrics should still slide under @code{TimeSignature} when an
@code{OctaveEight} is present.
"
}

\new Staff {
  \clef "treble_8"
  b
}
\addlyrics {
  \set stanza = "1."
  aaa
}
