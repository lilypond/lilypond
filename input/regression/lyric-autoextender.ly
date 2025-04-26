\version "2.25.26"

\header {
  texidoc = "Lyric extenders can be auto-generated whenever a
lyric syllable not followed by @samp{--} occurs in a melisma."
}

\layout {
  \set Lyrics.autoExtenders = ##t
}

\relative {
  c'4( e4) f8( g) a4
  g8( f e d c2)
  d8 e f g\noBeam a4 g8( f
  e1)
  f4 e d c
  \break
  d4 e f\melisma g
  a b c\melismaEnd a
  g8 f e d c e c4
}
\addlyrics {
  A me -- li -- sma, a _ _ me -- lis -- ma.
  No me -- lis -- ma.
  Ex -- pli -- cit me -- lis -- _ _ _ ma. _ _
}
