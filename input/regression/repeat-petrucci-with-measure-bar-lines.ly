\version "2.23.10"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "This test demonstrates an ancient repeat sign in the
Petrucci style, but with measure bar lines enabled.  A single bar line
should follow each repeat sign."
}

\layout {
  \context {
    \PetrucciStaff
    measureBarType = "|"
  }
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
