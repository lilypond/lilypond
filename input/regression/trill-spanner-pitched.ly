\version "2.7.13"

\header {

  texidoc = "Pitched trills are denoted by a small note head in
  parentheses following the main note. This note head is properly
  ledgered, and parentheses include the accidental."

}

\paper {
  raggedright = ##t
}

\relative {
  \pitchedTrill c4.\startTrillSpan es f\stopTrillSpan
}
