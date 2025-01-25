\version "2.25.28"

\header {
  texidoc = "@code{beamHalfMeasure = ##f} works as expected in a shortened
measure: the first ending should have no beams."
}

mus = \fixed c' {
  \time 3/4
  \set Timing.beamHalfMeasure = ##f
  \repeat volta 2 {
    \partial 8 c8 |
    c8 r8 r8 c8 c8 c8
    \alternative {
      {
        %% Adjusting measureLength instead of measurePosition (\partial)
        %% correctly begins the measure at position 0 and ends the measure at
        %% the desired point.
        \volta #'() \set Timing.measureLength = #5/8
        d8 r8 r8 d8 d8
        \volta #'() \set Timing.measureLength = #3/4
      }
      {
        c8 r8 r8 c8 c8-> r8
      }
    }
  }
  \fine
}

\score { \mus }
\score { \unfoldRepeats { \sectionLabel "Unfolded" \mus } }
