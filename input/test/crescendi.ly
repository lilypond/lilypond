
\version "2.3.4"
\header{
	texidoc="@cindex Crescendi
Crescendi can be printed in a number of different ways.
" }
\score{
\relative c''{
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

