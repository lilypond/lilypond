\version "2.3.8"
\header{ texidoc = "

@cindex Clef Manual Control

The positioning of glyph and note can be separated. @code{\clef} is 
a front-end, which keeps them together. All the notes in this example 
are central C."

}

\score {  {
  \set Staff.clefGlyph = #"clefs-F"
  \set Staff.clefPosition = #2
  c'4
  \set Staff.clefGlyph = #"clefs-G"
  c'4
  \set Staff.clefGlyph = #"clefs-C"

  c'4
	\set Staff.clefOctavation = #7 
  c'4
	\set Staff.clefOctavation = #0 
	\set Staff.clefPosition = #0
  c'4
	\clef "bass"
  c'4

}
	\paper{ raggedright = ##t }
}

