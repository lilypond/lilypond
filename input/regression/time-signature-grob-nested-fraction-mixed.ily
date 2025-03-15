\version "2.25.25"

\layout {
  \context {
    \Score
    \consists Measure_spanner_engraver
    \override TimeSignature.break-visibility = #end-of-line-invisible
  }
  \context {
    \Staff
    \remove Clef_engraver
  }
}

testMeasure = {
  \once \override Timing.TimeSignature.fraction = #'(5/2 . 4)
  \time 1/4
  s4

  \once \override Timing.TimeSignature.fraction = #'(2 . 8/3)
  \time 1/4
  s4

  \once \override Timing.TimeSignature.fraction = #'(1/2 . 3/4)
  \time 1/4
  s4

  \once \override Staff.BarLine.allow-span-bar = ##t
  \section
}

heightSweep = {
  \sectionLabel "(default)"
  \testMeasure

  \override Staff.TimeSignature.nested-fraction-relative-font-size = #-5
  \sectionLabel "font-size -5"
  \testMeasure

  \override Staff.TimeSignature.nested-fraction-relative-font-size = 0
  \sectionLabel "font-size 0"
  \testMeasure

  \break
}

\new StaffGroup \with {
  \override BarLine.allow-span-bar = ##f
} <<
  \new Staff \with { instrumentName = "(default)" } \heightSweep

  \new Staff \with {
    instrumentName = "vertical"
    \override TimeSignature.nested-fraction-orientation = #'vertical
  } \heightSweep

  \new Staff \with {
    instrumentName = "horizontal"
    \override TimeSignature.nested-fraction-orientation = #'horizontal
  } \heightSweep
>>
