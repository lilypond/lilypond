\version "2.25.33"

\header {
  categories = "Rhythms"

  texidoc = "
Odd time signatures (such as @qq{5/8}) can often be played as complex time
signatures (e.g. @qq{3/8 + 2/8}), which combine two or more inequal metrics.

LilyPond can make such music quite easy to read and play, by explicitly
printing the time signatures and adapting the automatic beaming behavior.
"

  doctitle = "Complex time signatures"
}


\relative c' {
  \timeAbbrev #'((2 8) (3 8))
  c8 d e fis gis
  c8 fis, gis e d
  c8 d e4 gis8
}
