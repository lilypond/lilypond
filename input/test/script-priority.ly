\version "1.5.68"
\header {
    texidoc = "Relative placements of different script types can be controlled
by overriding script-priority."
}
\paper { linewidth = -1. } 
\score{
    \context Staff \notes \relative g''{
	
 	\property Score.TextScript \override #'script-priority = #-100
	a4^\prall^#'((music (font-relative-size . -2)) "accidentals-1")

	
 	\property Score.Script \override #'script-priority = #-100
 	\property Score.TextScript \revert #'script-priority
	
	a4^\prall^#'((music (font-relative-size . -2)) "accidentals-1")
    }
}
