\version "1.9.2"
\header{ texidoc = "

@cindex Clef Manual Control

You can use the clef engraver by using @code{\property} directly.
@code{\clef} is merely a front-end to this. All the notes in this
example are central C."

}

\score { \notes {
  \property Staff.clefGlyph = #"clefs-F"
  \property Staff.clefPosition = #2
  c'4
  \property Staff.clefGlyph = #"clefs-G"
  c'4
  \property Staff.clefGlyph = #"clefs-C"

  c'4
	\property Staff.clefOctavation = #7 
  c'4
	\property Staff.clefOctavation = #0 
	\property Staff.clefPosition = #0
  c'4
	\clef "bass"
  c'4

}
	\paper{ raggedright = ##t }
}

