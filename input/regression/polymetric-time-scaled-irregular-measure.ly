\version "2.27.2"

\header {
  texidoc = "@code{\\measure} works under scaled @code{\\polymetric \\time}."
}

%% #(ly:set-option 'warning-as-error #t)

\layout {
  indent = 0
  \context {
    \Staff
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #all-visible
    \consists Bar_number_engraver
  }
}

\fixed c' <<
  \new Staff {
    \time 3/2
    \*3 f2 |
    \measure {
      \contextPropertyCheck Timing.measureLength #3/4
      f2 f4
    }
    \*3 f2 |
  }
  \new Staff {
    \scaleDurations 3/2 {
      \context Staff \polymetric \time 2/2
      \*8 f8 |
      \measure {
        \contextPropertyCheck Timing.measureLength #3/4
        \contextPropertyCheck Staff.measureLength \default
        \*4 f8
      }
      \*8 f8 |
    }
  }
  \new Staff {
    \scaleDurations 3/2 {
      \context Staff \polymetric \time 4/4
      \*8 f8 |
      \measure {
        \contextPropertyCheck Timing.measureLength #3/4
        \contextPropertyCheck Staff.measureLength \default
        \*4 f8
      }
      \*8 f8 |
    }
  }
>>
