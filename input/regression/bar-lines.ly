\version "2.19.21"

\header { texidoc = "Various types of bar lines can be drawn.

The dashes in a dashed bar line covers staff lines exactly. Dashed
bar lines between staves start and end on a half dash precisely.

The dots in a dotted bar line are in spaces.

A thick bar line is created by \bar \".\", which is consistent
with e.g. \bar \"|.\"

A ticked bar line is a short line of the same length as a staff
space, centered on the top-most bar line.

" }

\relative \new StaffGroup <<
  \new Staff = "1" {
    c'2 \bar "!" c
    s1
    c2 \bar ";" c
    s1
    c2 \bar "." c
    s1
    c2 \bar "'" c
  }
  \new Staff = "2" {
    c2 c
    s1
    c2 c
    s1
    c2 c
    s1
    c2 c
  }
>>

