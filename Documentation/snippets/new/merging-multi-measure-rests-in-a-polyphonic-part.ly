\version "2.17.18"

\header {
  lsrtags = "really-simple, rhythms, version-specific"

  texidoc = "
When using multi-measure rests in a polyphonic staff, the rests will be
placed differently depending on the voice they belong to. However they
can be printed on the same staff line, using the following setting.

"
  doctitle = "Merging multi-measure rests in a polyphonic part"
}

normalPos = \revert MultiMeasureRest.direction

{
  <<
    {
      c''1
      R1
      c''1
      \normalPos
      R1
    }
    \\
    {
      c'1
      R1
      c'1
      \normalPos
      R1
    }
  >>
}
