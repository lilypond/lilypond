\version "1.7.18"
\header {
    texidoc = "@cindex Dynamic Absolute Volume
Absolute dynamics have effect in MIDI files.
"
}


\score{
\notes\relative c''{
%segfault in engraver
a1-\ppp 
a1-\pp
a-\p
a-\mp
a-\mf
a-\f
a-\ff
a\fff
a\sf
}
\paper{ raggedright = ##t }
\midi{
\tempo 1 = 60
}
}

