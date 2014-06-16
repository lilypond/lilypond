\version "2.19.4"

\header {
  texidoc = "Time signature changes in midmeasure generate warnings,
except in an anacrusis or when @code{ignoreBarChecks} is true.
@code{measurePosition} is reset to 0, so a full measure follows, regardless
of the original position.

This example should end at bar 3, with no barline before the 2/4."
}

#(ly:expect-warning (_ "\\time in mid-measure at 1/4"))

\score {
  \relative {
    \override Score.BarNumber.break-visibility = #all-visible
    \partial 8 \time 2/4
    a'8 | d4
    \time 6/8 \partial 4.
    cis8 b a | g4. \barNumberCheck 2
    \set Timing.ignoreBarChecks = ##t
    \time 12/8
    fis4. e d
    \time 2/4
    \set Timing.ignoreBarChecks = ##f
    a'2 | \barNumberCheck 3
  }
}
