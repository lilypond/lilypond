\version "1.7.18"
% possible rename to clef-manual-control
\header{ texidoc = "@cindex Clef Manual Control
You can use the clef engraver by setting \property directly.  \clef
is merely a front-end to this. " }

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

