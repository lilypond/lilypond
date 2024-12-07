\version "2.25.23"

\header {
  texidoc = "When @code{beamMaximumSubdivision} <
@code{beamMinimumSubdivision}, the subdivision depth should
respect the limit imposed by
@code{beamMaximumSubdivision}
even if it is not metrically correct.
"
}

\layout {
  indent = 0
  ragged-right = ##t
}

{
  \stopStaff
  \omit Staff.Clef
  \omit Staff.TimeSignature

  \set Timing.beamExceptions = #'()
  \set Timing.beatStructure = 1
  \set Timing.beatBase = 1

  \set subdivideBeams = ##t
  \set beamMinimumSubdivision = #1/8
  \set beamMaximumSubdivision = #1/16
  \repeat unfold 64 c64
}
