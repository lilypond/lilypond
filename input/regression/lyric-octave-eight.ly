\version "2.27.0"

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
  \stanza "1."
  aaa
}
