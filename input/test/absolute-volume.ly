\header {
    texidoc = "Absolute dynamics have effect in MIDI files."
}

\version "1.7.16"

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
\paper{
}
\midi{
\tempo 1 = 60
}
}
%% new-chords-done %%
