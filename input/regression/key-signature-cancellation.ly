\header {

  texidoc = "Key cancellation signs consists of naturals for pitches
  that are not in the new key signature. Naturals get a little padding
  so the stems don't collide."

}

\version "2.16.0"

\layout {
  ragged-right = ##t
}

{

  \key a \major
  g'1
  \key f \minor
  g'1
  \key cis \major
  g'1
  \key c \major
  g'1
}
