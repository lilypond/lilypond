\header {

  texidoc = "Shape notes can be set to work properly in minor keys."

}
\version "2.16.0"

\relative c' {
  \key c \major
  \sacredHarpHeads
  c2^"C major" d | e f | g a | b c |
  \key a \minor
  \sacredHarpHeadsMinor
  a2^"A minor" b | c d | e f | g a |
  \sacredHarpHeads
  c,,2^"A minor with major heads" d | e f | g a | b c |
}

