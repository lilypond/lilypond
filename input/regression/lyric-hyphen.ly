\version "2.6.0"
\header {
    texidoc = "In lyrics, hyphens may be used."
}
\layout {

    raggedright= ##t
}

<<
    \context Staff  { c' (c') c'( c') }
    \context Lyrics \lyricmode { bla -- alb xxx -- yyy }
>>






