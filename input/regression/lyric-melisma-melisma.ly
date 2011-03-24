\version "2.13.55"
\header {
  texidoc = "A syllable aligned with a melisma delimited with
@code{\melisma} and @code{\melismaEnd} should be left-aligned.
"
}

\relative c' {
  c4 c c16\melisma d e f \melismaEnd g4
}
\addlyrics { ha ha looong __ ho }
