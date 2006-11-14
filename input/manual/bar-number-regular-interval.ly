\header {
    texidoc = "

Bar numbers can be printed at regular intervals, inside a box or a circle.

" }

\version "2.10.0"

\relative c'{
   \override Score.BarNumber  #'break-visibility = #end-of-line-invisible
   \set Score.barNumberVisibility = #(every-nth-bar-number-visible 4)
   \override Score.BarNumber #'font-size = #2

   \override Score.BarNumber  #'stencil
   = #(make-stencil-boxer 0.1 0.25 ly:text-interface::print)
   \repeat unfold 5 { c1 } \bar "|"

   \override Score.BarNumber  #'stencil
   = #(make-stencil-circler 0.1 0.25 ly:text-interface::print)
   \repeat unfold 4 { c1 } \bar "|."
}


