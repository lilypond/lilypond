\version "2.3.4"
\header {
    texidoc = "In lyrics, hyphens may be used."
}
\paper {

    raggedright= ##t
}

<<
    \context Staff  { c' (c') c'( c') }
    \context Lyrics \lyrics { bla -- alb xxx -- yyy }
>>






