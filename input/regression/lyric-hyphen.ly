\version "2.7.32"
\header {
    texidoc = "In lyrics, hyphens may be used."
}
\layout {

    ragged-right= ##t
}

<<
    \context Staff  { c' (c') c'( c') }
    \context Lyrics \lyricmode { bla -- alb xxx -- yyy }
>>






