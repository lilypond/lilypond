\version "2.27.2"

\header {
  texidoc = "@code{\\setMeasureLengthFromHere} can be used within
@code{\\scaleDurations}.  Its duration is scaled like any other duration.

Measure@tie{}2 is only half as long as the time signature indicates."
}

#(ly:set-option 'warning-as-error #t)

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
    { % an irregular measure
      \setMeasureLengthFromHere 2.
      \contextPropertyCheck Timing.measureLength #3/4
      f2 f4 |
      \setDefaultMeasureLength
    }
    \*3 f2 |
  }
  \new Staff {
    \scaleDurations 3/2 {
      \context Staff \polymetric \time 2/2
      \*8 f8 |
      { % an irregular measure
        \setMeasureLengthFromHere 2
        \contextPropertyCheck Timing.measureLength #3/4
        \contextPropertyCheck Staff.measureLength \default
        \*4 f8 |
        \setDefaultMeasureLength
      }
      \*8 f8 |
    }
  }
  \new Staff {
    \scaleDurations 3/2 {
      \context Staff \polymetric \time 4/4
      \*8 f8 |
      { % an irregular measure
        \setMeasureLengthFromHere 2
        \contextPropertyCheck Timing.measureLength #3/4
        \contextPropertyCheck Staff.measureLength \default
        \*4 f8 |
        \setDefaultMeasureLength
      }
      \*8 f8 |
    }
  }
>>
