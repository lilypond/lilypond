\version "2.23.6"

% Helper for barNumberVisibility tests

music = \relative {
  \repeat unfold 3 { c'2 \bar "" \break 2 2 2 }
}

testVisibility =
#(define-music-function (visibility) (procedure?)
   #{
     \set Score.barNumberVisibility = #visibility
     \override Score.BarNumber.break-visibility = #end-of-line-invisible
     \bar ""
     #music
   #})
