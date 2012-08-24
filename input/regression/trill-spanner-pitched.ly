\version "2.16.0"

\header {

  texidoc = "Pitched trills are denoted by a small note head in
  parentheses following the main note. This note head is properly
  ledgered, and parentheses include the accidental."

}

\paper {
  ragged-right = ##t
}

\relative c' {
  \pitchedTrill c4.\startTrillSpan es f\stopTrillSpan
}
