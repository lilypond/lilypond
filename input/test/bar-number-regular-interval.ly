\header {
    texidoc = "

Bar numbers can also be printed at regular intervals.

" }

\version "2.1.21"

\score {
    \context Staff \notes \transpose  c c' {
	\property Score.BarNumber \override #'break-visibility = #end-of-line-invisible
	\property Score.barNumberVisibility = #(every-nth-bar-number-visible 4)
	\property Score.BarNumber \override #'print-function =
	#(make-molecule-boxer 0.1 0.25 0.25 Text_item::print)
	\property Score.BarNumber \override #'font-size = #2
	
	\repeat unfold 9 { c1 } \bar "|."
    }
}


