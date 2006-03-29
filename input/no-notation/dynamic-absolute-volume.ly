
\version "2.7.39"
\header {
    texidoc = "@cindex Dynamic Absolute Volume
Absolute dynamics have an effect on MIDI files.
"
}


\score{
\relative c''{
%segfault in engraver
a1\ppp 
a1\pp
a\p
a\mp
a\mf
a\f
a\ff
a\fff
a\sf
}
\layout{ ragged-right = ##t }
\midi{
\tempo 1 = 60
}
}

