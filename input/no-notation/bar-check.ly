\version "2.8.0"

\header {

texidoc = "skipTypesetting doesn't affect bar checks."

}

\score {  {
	\set Score.skipTypesetting = ##t
	c4 c4
	|
	c4 c4 }}
