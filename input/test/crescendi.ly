
\version "1.9.4"
\header{
	texidoc="@cindex Crescendi
LilyPond can print crescendi in a number of different ways.
" }
\score{
\notes\relative c''{
a1\fff\> a\pp\!
a\< a\!
\property Voice.crescendoText = \markup { \italic \bold "cresc." }
\property Voice.crescendoSpanner = #'dashed-line
a\mf\< a a\! 
a\< a\!
}
\paper{
raggedright = ##t
}
\midi{
\tempo 1 = 60
}
}

