\version "2.23.6"

\header { texidoc = "Various types of bar lines can be drawn.

The dashes in a dashed bar line covers staff lines exactly. Dashed
bar lines between staves start and end on a half dash precisely.

The dots in a dotted bar line are in spaces.

A thick bar line is created by \bar \".\", which is consistent
with e.g. \bar \"|.\"

A tick bar line is a short line of the same length as a staff
space, centered on the top-most bar line.

A short bar line has a height of half the height of the staff, rounded
up to an integer number of staff spaces.  It is usually centered
vertically, but on short staves, it is shifted down to distinguish it
from a normal bar line."
}

bars = {
  s2 \bar "!" s
  s1
  s2 \bar ";" s
  s1
  s2 \bar "." s
  s1
  s2 \bar "'" s
  s1
  s2 \bar "," s
}

music = \fixed c' {
  \clef "alto"
  c2 c
  s1
  c2 c
  s1
  c2 c
  s1
  c2 c
  s1
  c2 c
}

<<
  \new StaffGroup <<
    \new Staff \with {
      \override StaffSymbol.line-count = #6
    } <<
      \bars \music
    >>

    \new Staff \music

    \new Staff \with {
      \override StaffSymbol.staff-space = 2
    } \music
  >>

  \new ChoirStaff <<
    \new Staff \with {
      \override StaffSymbol.line-positions = #'(-4 -2 2 5)
    } \music

    \new Staff \with {
      \override StaffSymbol.line-positions = #'(-2 0 2 4)
    } \music

    \new Staff \with {
      \override StaffSymbol.line-count = #4
    } \music

    \new Staff \with {
      \override StaffSymbol.line-positions = #'(-4 0 4)
      \override BarLine.bar-extent = #'(-3 . 3)
    } \music

    \new Staff \with {
      \override StaffSymbol.line-positions = #'(-4 0 4)
    } \music

    \new Staff \with {
      \override StaffSymbol.line-count = #3
    } \music
    \new Staff \with {
      \override StaffSymbol.line-count = #2
    } \music

    \new Staff \with {
      \override StaffSymbol.line-count = #2
      \override BarLine.bar-extent = #'(-0.5 . 0.5)
    } \music

    \new Staff \with {
      \override StaffSymbol.line-count = #1
    } \music

    \new Staff \with {
      \override StaffSymbol.line-count = #0
    } \music
  >>
>>
