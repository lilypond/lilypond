\version "2.1.22"
\header{ texidoc = "

@cindex Clef Manual Control

You can use the clef engraver by using @code{\property} directly.
@code{\clef} is merely a front-end to this. All the notes in this
example are central C."

}

\score { \notes {
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

