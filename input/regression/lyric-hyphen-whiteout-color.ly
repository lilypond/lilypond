\version "2.25.22"

\header {
  texidoc = "Using the @code{whiteout-color} property it is possible to
adjust the whiteout of lyric hyphens."
}

{ g'1 g' g' g' }
\addlyrics {
  \override LyricHyphen.whiteout = #2
  \override LyricHyphen.whiteout-color = "red"
  Red -- dash.
  \override LyricHyphen.whiteout = #5
  \override LyricHyphen.whiteout-color = \cyan
  Cyan -- dash.
}
