\version "2.23.7"

\layout {
  \context {
    \Staff
    \omit Clef
  }
}

test =
#(define-music-function (bar-extent) (number-pair?)
  #{
    \fixed c' {
      \stopStaff
      s4
      \override Staff.BarLine.bar-extent = #bar-extent
      \startStaff
      \bar "|" f4 \bar \testBar e'4 \bar "|"
    }
  #} )

sweepBarExtent = {
  \test #'(-0.5 . 0.5)
  \test #'(-1.0 . 1.0)
  \test #'(-1.5 . 1.5)
  \test #'(-2.0 . 2.0)
  \test #'(-2.5 . 2.5)
  \test #'(-3.0 . 3.0)
}

<<

  \new Staff \with {
    \override StaffSymbol.line-positions = #'(-4 -2 0 2 4)
  } {
    \sweepBarExtent
  }

  \new Staff \with {
    \override StaffSymbol.line-positions = #'(-4 -2 2 4)
  } {
    \sweepBarExtent
  }

  \new Staff \with {
    \override StaffSymbol.line-positions = #'(-3 -1 1 3)
  } {
    \sweepBarExtent
  }

  \new Staff \with {
    %% can't avoid dotting a line: hit one rather than two
    \override StaffSymbol.line-positions = #'(-3 0 3)
  } {
    \sweepBarExtent
  }

  \new Staff \with {
    \override StaffSymbol.line-positions = #'(-2 2)
  } {
    \sweepBarExtent
  }

  \new Staff \with {
    \override StaffSymbol.line-positions = #'(-1 1)
  } {
    \sweepBarExtent
  }

  \new Staff \with {
    \override StaffSymbol.line-positions = #'(0)
  } {
    \sweepBarExtent
  }

  \new Staff \with {
    \override StaffSymbol.line-count = 0
  } {
    \sweepBarExtent
  }

>>
