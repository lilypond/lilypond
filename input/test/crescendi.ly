
\version "2.1.26"
\header{
	texidoc="@cindex Crescendi
LilyPond can print crescendi in a number of different ways.
" }
\score{
\notes\relative c''{
a1\fff\> a\pp\!
a\< a\!
\set crescendoText = \markup { \italic \bold "cresc." }
\set crescendoSpanner = #'dashed-line
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

