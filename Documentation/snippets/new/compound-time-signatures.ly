\version "2.17.15"

\header {
  lsrtags = "rhythms"

  texidoc = "
Odd 20th century time signatures (such as \\\"5/8\\\") can often be
played as compound time signatures (e.g. \\\"3/8 + 2/8\\\"), which
combine two or more inequal metrics. LilyPond can make such music quite
easy to read and play, by explicitly printing the compound time
signatures and adapting the automatic beaming behavior.

"
  doctitle = "Compound time signatures"
}

\relative c' {
  \compoundMeter #'((2 8) (3 8))
  c8 d e fis gis
  c8 fis, gis e d
  c8 d e4 gis8
}
