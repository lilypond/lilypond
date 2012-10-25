\header
{
  texidoc = "The @code{\\tweak} function can be used in Lyrics.  Where
confusion of lyric words with grob names is possible, explicit use of
@code{\\markup} can be used for resolving the ambiguity."
}

\version "2.17.6"
\paper {
  ragged-right = ##t
}

\new Lyrics \lyricmode
{
  \markup \raise #1 \rotate #30 One 4
  \tweak extra-offset #'(0 . 2) \markup fish,
  \markup \raise #1 \rotate #-30 two fish,
  \tweak color #red \markup red fish,
  \tweak color #blue \markup blue fish.
}
