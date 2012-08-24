\version "2.16.0"

\header {

  texidoc = "Prefatory items maintain sufficient separation from
  musical notation for readability, even in tight spacing.
  The notes should remain generally on the correct side of the
  time signature, key signature and barlines.  A key change to
  G major should be legible."

}

\new Staff <<
  \relative c''' {
    \key f \major
    es4 c c c \bar "||" \noBreak
    \key g \major
    <gis cis,>4 r2. \bar "|."
    } \\
  \relative c' {
    R1
    r4 cis a a
  }
>>
\layout {
  line-width = 5\cm % impossibly narrow to induce tight spacing
}
