\header {
    texidoc = "

Bar numbers can also be printed at regular intervals.

" }

\version "2.1.26"

\score {
    \context Staff \notes \transpose  c c' {
	\override Score.BarNumber  #'break-visibility = #end-of-line-invisible
	\set Score.barNumberVisibility = #(every-nth-bar-number-visible 4)
	\override Score.BarNumber  #'print-function =
	#(make-stencil-boxer 0.1 0.25 0.25 Text_item::print)
	\override Score.BarNumber  #'font-size = #2
	
	\repeat unfold 9 { c1 } \bar "|."
    }
}


