
\version "2.7.32"
\header {

    texidoc = "@cindex Bars Always

By setting @code{barAlways} and @code{defaultBarType}, barlines may be inserted automatically everywhere."

}


\score {
	 \relative c''{
		\set Score.barAlways = ##t
		\set Score.defaultBarType = ":|:"
		c4 c4 c4 c4 }
	\layout{ragged-right = ##t}
}

