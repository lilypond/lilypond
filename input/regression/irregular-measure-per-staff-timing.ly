\version "2.27.2"

\header {
  texidoc = "When @code{\\enablePerStaffTiming} is used, @code{\\measure}
operates independently in each staff."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \enablePerStaffTiming
  \context {
    \Staff
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #all-visible
    \consists Bar_number_engraver
  }
}

\fixed c' <<
  \new Staff {
    \time 4/4
    f4 f f f |
    \measure { f4 f f }
    f4 f f f |
  }
  \new Staff {
    \time 3/4
    f4 f f |
    \measure { f4 f f f f }
    f4 f f |
  }
>>
