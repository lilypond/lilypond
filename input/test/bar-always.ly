
\version "2.1.26"
\header {

    texidoc = "@cindex Bars Always

By setting @code{barAlways} and @code{defaultBarType,} you can automatically insert barlines everywhere."

}


\score {
	\notes \relative c''{
		\set Score.barAlways = ##t
		\set Score.defaultBarType = ":|:"
		c4 c4 c4 c4 }
	\paper{raggedright = ##t}
}

