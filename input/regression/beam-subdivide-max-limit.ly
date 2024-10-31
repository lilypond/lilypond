\version "2.25.22"

\header {
  texidoc = "When @code{maximumBeamSubdivisionInterval} <
@code{minimumBeamSubdivisionInterval}, the subdivision depth should
respect the limit imposed by @code{maximumBeamSubdivisionInterval}
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
  \set minimumBeamSubdivisionInterval = \musicLength 8
  \set maximumBeamSubdivisionInterval = \musicLength 16
  \repeat unfold 64 c64
}
