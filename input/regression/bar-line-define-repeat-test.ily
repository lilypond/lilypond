\version "2.23.6"

\layout {
  indent = 0
  ragged-right = ##t

  \context {
    \Score
    %% A test may cover \inStaffSegno.  Omit outside-staff marks to
    %% sharpen the focus on the bar lines.
    \omit SegnoMark
  }
}

\score {
  %% Repeats that are not aligned to measure boundaries potentially
  %% involve underlyingRepeatBarType in the test.
  \new Staff \fixed c' {
    r2.
    \repeat volta 2 {
      \testBars % at start-repeat bar
      r4 | r2.
    }
    \repeat volta 2 {
      \testBars % at double-repeat bar
      r4 | r2.
      \testBars % at end-repeat bar
    }
    r4 | r2.
    \testBars % not at repeat bar
    r4
  }
}
