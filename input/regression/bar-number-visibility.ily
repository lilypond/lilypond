\version "2.25.35"

% Helper for barNumberVisibility tests

music = \relative {
  \*3 { c'2 \bar "" \break 2 2 2 }
}

testVisibility =
#(define-music-function (visibility) (procedure?)
   #{
     \set Score.barNumberVisibility = #visibility
     \override Score.BarNumber.break-visibility = #end-of-line-invisible
     \bar ""
     #music
   #})
