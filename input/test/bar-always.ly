
\version "2.3.17"
\header {

    texidoc = "@cindex Bars Always

By setting @code{barAlways} and @code{defaultBarType}, barlines may be inserted automatically everywhere."

}


\score {
	 \relative c''{
		\set Score.barAlways = ##t
		\set Score.defaultBarType = ":|:"
		c4 c4 c4 c4 }
	\paper{raggedright = ##t}
}

