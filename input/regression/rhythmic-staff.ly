\version "2.18.0"
\header
{
  texidoc = "In @code{RhythmicStaff}, stems should go up, and bar lines should
extend one staff space above and below the line.  The whole rest hangs from the
rhythmic staff."
}

\layout { ragged-right = ##t }

\context RhythmicStaff
{
    r4 c4. c8 r8 c8 | c2 r2 | r1
}
