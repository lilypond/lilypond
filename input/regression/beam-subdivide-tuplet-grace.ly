\version "2.25.19"

\header {
  texidoc = "Beamed grace notes within a tuplet should be subdivided
independently from the tuplet.  Each of the 3 beams of the second
tuplet, where beam subdivision is enabled, should have 2 subdivisions
at 16th level, 1 subdivision at 8th level."
}

\paper {
  indent = 0
  ragged-right = ##t
}

{
  \time 1/4
  \stopStaff
  \omit Score.BarNumber
  \omit Staff.Clef
  \omit Staff.TimeSignature

  \tuplet 3/2 { \repeat unfold 3 { \grace { \repeat unfold 8 a32 } b8 } } \break
  \set subdivideBeams = ##t
  \tuplet 3/2 { \repeat unfold 3 { \grace { \repeat unfold 8 a32 } b8 } } \break
}
