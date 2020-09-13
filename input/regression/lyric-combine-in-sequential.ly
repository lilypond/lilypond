\version "2.21.7"

\header {
  texidoc = "This tests @code{\\lyricsto} as the first element of
sequential music."
}

<<
  \context Voice = V f'1
  \context Lyrics { \lyricsto V { \lyricmode { eff } } }
>>
