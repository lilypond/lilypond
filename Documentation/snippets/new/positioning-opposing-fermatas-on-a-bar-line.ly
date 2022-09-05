\version "2.23.13"

\header {
  lsrtags = "rhythms"

  texidoc = "
This snippet demonstrates a command that prints fermatas both above
and below a bar line.  If there would not otherwise be a bar line, it
adds a double bar line.  Semantically, the command codes a
longer-than-normal caesura, which might be considered misuse depending
on the situation.
"

  doctitle = "Positioning opposing fermatas on a bar line"
}

twoWayFermata = {
  \once \set Staff.caesuraType = #'((underlying-bar-line . "||"))
  \once \set Staff.caesuraTypeTransform = ##f
  \caesura ^\fermata _\fermata
}

music = {
  f'1 \twoWayFermata
  R1
  f'2 \twoWayFermata f'2
  R1
  b'1 \twoWayFermata \fine
}

\new GrandStaff <<
  \new Staff \music
  \new Staff \music
>>
