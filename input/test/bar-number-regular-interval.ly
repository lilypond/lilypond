\header {
    texidoc = "

Bar numbers can also be printed at regular intervals.

" }

\version "1.9.1"

\score {
    \context Staff \notes \transpose  c c' {
	\property Score.BarNumber \override #'break-visibility = #end-of-line-invisible
	\property Score.barNumberVisibility = #(every-nth-bar-number-visible 4)
	\property Score.BarNumber \override #'molecule-callback =
	#(make-molecule-boxer 0.1 0.25 0.25 Text_item::brew_molecule)
	\property Score.BarNumber \override #'font-relative-size = #1
	
	\repeat unfold 9 { c1 } \bar "|."
    }
}


