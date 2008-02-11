\version "2.10.0"
\header {
    texidoc = "In lyrics, hyphens may be used."
}
\layout {

    ragged-right= ##t
}

<<
    \context Staff  { c' (c') c'( c') }
    \context Lyrics \lyricmode {
      \override Lyrics . LyricSpace #'minimum-distance = #5.0
      a -- b x -- y
    }
>>






