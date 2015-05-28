
\version "2.19.21"

\header {
  texidoc = "Stem length and stem-begin-position can be set manually.
"
}

\relative {
  \autoBeamOff
  \stemUp
  a8 aes''8
  \override Stem.length = #20
  a,,8
  \revert Stem.length
  aes''!8
  \stemNeutral
  \override Stem.stem-begin-position = #-2
  c,8
}
