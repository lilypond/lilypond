\version "1.7.18"
\header {

    texidoc = "@cindex Bars Always
By setting barAlways and defaultBarType, you can
automatically insert barlines everywhere.
" }


\score {
	\notes \relative c''{
		\property Score.barAlways = ##t
		\property Score.defaultBarType = ":|:"
		c4 c4 c4 c4 }
	\paper{raggedright = ##t}
}

