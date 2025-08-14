\version "2.25.28"

\header {
  texidoc = "@code{\\time #'(@var{x} . 1/2)} sets timing properties using a
double whole note as the beat base."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    \override TimeSignature.denominator-style = #'note
  }
}

\fixed c' {
  \time #'(3 . 1/2)
  \contextPropertyCheck Timing.beatBase 2
  \contextPropertyCheck Timing.beatStructure #'(1 1 1)
  \contextPropertyCheck Timing.measureLength 6
  \contextPropertyCheck Timing.timeSignature #'(3 . 1/2)
  <<
    \context Staff = "A" { c\breve e g }
    \context Staff = "B" { R\breve*3 }
  >>

  \time #'(4 . 1/2)
  \contextPropertyCheck Timing.beatBase 2
  \contextPropertyCheck Timing.beatStructure #'(1 1 1 1)
  \contextPropertyCheck Timing.measureLength 8
  \contextPropertyCheck Timing.timeSignature #'(4 . 1/2)
  <<
    \context Staff = "A" { c\breve e g a }
    \context Staff = "B" { R\breve*4 }
  >>

  \time #'(6 . 1/2)
  \contextPropertyCheck Timing.beatBase 2
  \contextPropertyCheck Timing.beatStructure #'(3 3)
  \contextPropertyCheck Timing.measureLength 12
  \contextPropertyCheck Timing.timeSignature #'(6 . 1/2)
  <<
    \context Staff = "A" { c\breve d e f g a }
    \context Staff = "B" { R\breve*6 }
  >>
}
