\version "2.21.0"

\header {
  texidoc = "Users can override the @code{text} property of
@code{ChordName}.
"
}

\new ChordNames \chordmode {
  a b c:7
  \once \override ChordName.text = "foo"
  d
}
