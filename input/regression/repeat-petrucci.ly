\version "2.23.8"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "This test demonstrates an ancient repeat sign in the
Petrucci style.  The sign consists of 1 to@tie{}4 short strokes
between repeat dots, with the number of strokes indicating the number
of times the preceding section is to be performed.  The number of
strokes is determined by the argument to @code{\\repeat volta}, and a
count higher than@tie{}4 falls back on a modern-looking sign with two
long strokes.  Despite appearances, these repeat signs are not bar
lines.

A double bar line should follow the first note.  A repeat sign should
follow each following note: modern, 4@tie{}strokes, 3@tie{}strokes,
2@tie{}strokes, 1@tie{}stroke."
}

music = \fixed c' {
  c1 |
  \repeat volta 5 d1
  \repeat volta 4 e1
  \repeat volta 3 f1
  \repeat volta 2 g1
  \repeat volta 1 a1
}

\new StaffGroup <<
  \new PetrucciStaff \music
  \new PetrucciStaff \music
>>
