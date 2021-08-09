\version "2.19.21"

\header {

  texidoc = "Prefatory items maintain sufficient separation from
  musical notation for readability, even in tight spacing.
  The notes should remain generally on the correct side of the
  time signature, key signature and bar lines.  A key change to
  G@tie{}major should be legible."

}

\new Staff <<
  \relative {
    \key f \major
    es'''4 c c c \bar "||" \noBreak
    \key g \major
    <gis cis,>4 r2. \bar "|."
    } \\
  \relative {
    R1
    r4 cis' a a
  }
>>
\layout {
  line-width = 5\cm % impossibly narrow to induce tight spacing
}
