\version "1.3.146"

\score{
\notes\relative c''{
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
\paper{
}
\midi{
\tempo 1 = 60
}
}
