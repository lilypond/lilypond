\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "A segno at a line break appears at the beginning of the
line."
}

\layout {
  ragged-right = ##t
}

{
  R1 \break |
  \segnoMark \default R1 |
}
