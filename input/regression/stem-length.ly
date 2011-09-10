
\version "2.15.9"

\header {
  texidoc = "Stem length and stem-begin-position can be set manually.
"
}

\relative c' {
  \autoBeamOff
  \stemUp
  a8 aes''8
  \override Stem #'length = #20
  a,,8
  \revert Stem #'length
  aes''!8
  \stemNeutral
  \override Stem #'stem-begin-position = #-2
  c,8
}
