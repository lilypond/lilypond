\version "2.23.7"

\layout {
  \context {
    \Staff
    \omit Clef
  }
}

test =
#(define-music-function (extra-bar-extent) (number?)
  #{
    \fixed c' {
      \stopStaff
      s4
      \override Staff.BarLine.bar-extent =
        #(lambda (grob)
          (interval-widen (ly:bar-line::calc-bar-extent grob) extra-bar-extent))
      \startStaff
      \bar "|" f4 \bar \testBar e'4 \bar "|"
    }
  #} )

sweepBarExtent = {
  \test -1.0
  \test -0.5
  \test 0.0
  \test 0.5
  \test 1.0
}

testPianoStaff =
#(define-music-function (rh-lines lh-lines) (index? index?)
  #{
    \new PianoStaff <<
      \new Staff \with {
        \override StaffSymbol.line-count = #rh-lines
      } {
        \sweepBarExtent
      }

      \new Staff \with {
        \override StaffSymbol.line-count = #lh-lines
      } {
        \sweepBarExtent
      }
    >>
  #} )

<<
  \testPianoStaff 5 5
  \testPianoStaff 5 4
  \testPianoStaff 4 5
  \testPianoStaff 4 4
  \testPianoStaff 1 1
>>
