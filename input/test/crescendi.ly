\version "1.7.18"
\header{
	texidoc="@cindex Crescendi
Lilypond can print crescendi in a number of different ways.
" }
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
