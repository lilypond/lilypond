\version "2.25.32"

\header {
  texidoc = "In this test, the Score time signature is 4/4.  In the staff with
scaled 3/4 time, the automatic beamer should honor the
@code{Timing@/.beamHalfMeasure} option as it would for unscaled 3/4 time."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  indent = 0
}

\fixed c' <<
  \new Staff {
    \repeat unfold 16 c2
  }
  \new Staff {
    \scaleDurations 4/3 {
      \context Staff \polymetric \time 3/4

      \sectionLabel "Timing.beamHalfMeasure = ##f"
      \set Timing.beamHalfMeasure = ##f
      r4. c8 c c |
      c8 c r c c r |
      r8 c c c c c |
      c c c r r4 |

      \break

      \sectionLabel "Timing.beamHalfMeasure = ##t"
      \set Timing.beamHalfMeasure = ##t
      r4. c8 c c |
      c8 c r c c r |
      r8 c c c c c |
      c c c r r4 |
    }
  }
>>
