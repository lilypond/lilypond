\version "2.23.3"

\header {
  texidoc = "User tweaks to @code{OttavaBracket.direction} are
honored in all cases.

In this test, marcato marks show the expected placement."
}

\layout {
  \context {
    \Voice
    \override Script.outside-staff-priority = 0
  }
}

{
  \ottava 1
  c''1^\marcato
  \ottava 0
  c'1
  \ottava -1
  c1_\marcato
  \ottava 0
}

{
  \once \override Staff.OttavaBracket.direction = #DOWN
  \ottava 1
  c''1_\marcato
  \ottava 0
  c'1
  \once \override Staff.OttavaBracket.direction = #UP
  \ottava -1
  c1^\marcato
  \ottava 0
}
