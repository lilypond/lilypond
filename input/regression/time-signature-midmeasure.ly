\version "2.23.9"

\header {
  texidoc = "Mid-measure time signature changes must be accompanied by
\\partial.

In this example, no bar numbers should be omitted or repeated, and all
double bar lines should have parenthesized bar numbers consistent with
the single bar lines.  Both scores should look identical.

@itemize @bullet
@item \\time 2/4 occurs at a negative position
@item \\time 6/8 occurs at a position less than the new measure length
@item \\time 3/8 occurs at a position equal to the new measure length
@item \\time 3/16 occurs at a position greater than the new measure length
@end itemize"
}

\score {
  \relative {
    \set Score.barNumberVisibility = #all-bar-numbers-visible
    \override Score.BarNumber.break-visibility = #all-visible
    \time 2/4 \partial 8
    a'8 | d4
    \bar "||" \time 6/8 \partial 4.
    cis8 b a | \barNumberCheck 2 g2. | \barNumberCheck 3 g4.
    \bar "||" \time 3/8 \partial 4.
    f4. | \barNumberCheck 4 f4. | \barNumberCheck 5 f4
    \bar "||" \time 3/16 \partial 8
    fis8 | \barNumberCheck 6 g16 a g | \barNumberCheck 7
  }
}

\score {
  \relative {
    \set Score.barNumberVisibility = #all-bar-numbers-visible
    \override Score.BarNumber.break-visibility = #all-visible
    \partial 8 \time 2/4
    a'8 | d4
    \bar "||" \partial 4. \time 6/8
    cis8 b a | \barNumberCheck 2 g2. | \barNumberCheck 3 g4.
    \bar "||" \partial 4. \time 3/8
    f4. | \barNumberCheck 4 f4. | \barNumberCheck 5 f4
    \bar "||" \partial 8 \time 3/16
    fis8 | \barNumberCheck 6 g16 a g | \barNumberCheck 7
  }
}
