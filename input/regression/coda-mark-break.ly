\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "A coda mark at a line break appears at the end of the
line."
}

\layout {
  ragged-right = ##t
}

{
  R1 \break |
  \codaMark \default R1 |
}
