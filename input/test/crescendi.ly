\version "1.7.6"

\score{
\notes\relative c''{
a1\fff\> \!a-\pp
a\< \!a
\property Voice.crescendoText = "cresc."
\property Voice.crescendoSpanner = #'dashed-line
a-\mf\< a \!a 
a\< \!a
}
\paper{
}
\midi{
\tempo 1 = 60
}
}
%% new-chords-done %%
