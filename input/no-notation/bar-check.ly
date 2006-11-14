\version "2.10.0"

\header {

texidoc = "skipTypesetting doesn't affect bar checks."

}

\score {  {
	\set Score.skipTypesetting = ##t
	c4 c4
	|
	c4 c4 }}
