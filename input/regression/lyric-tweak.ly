\header
{
  texidoc = "The @code{\\tweak} function can be used in Lyrics."
}

\version "2.19.25"
\paper {
  ragged-right = ##t
}

\new Lyrics \lyricmode
{
  \markup \raise #1 \rotate #30 One 4
  \tweak extra-offset #'(0 . 2) fish,
  \markup \raise #1 \rotate #-30 two fish,
  \tweak color #red red fish,
  \tweak color #blue blue fish.
}
