\version "2.25.35"

\header {
  texidoc = "Slashed beams work nicely with different sizes caused by
@code{\magnifyStaff} or @code{layout-set-staff-size}."
}

mus = {
  \time 7/4
  \stemUp
  \override Beam.stencil = #beam::slashed-stencil
  \*4 b'16
  \once \override Beam.positions = #'(3 . 4)
  \*4 b'
  \once \override Beam.positions = #'(4 . 3)
  \*4 b'

  \stemDown
  \*4 b'
  \once \override Beam.positions = #'(-3 . -4)
  \*4 b'
  \once \override Beam.positions = #'(-4 . -3)
  \*4 b'

  \grace { \*4 b' } b'4

}

music =
  <<
    \new Staff \with { \magnifyStaff #4/5 } \mus
    \new Staff \with { \magnifyStaff #1/1 } \mus
    \new Staff \with { \magnifyStaff #5/4 } \mus
  >>

\score {
  \music
  \layout {
    #(layout-set-staff-size 24)
  }
}

\score {
  \music
  \layout {
    #(layout-set-staff-size 16)
    \override Beam.details.slash-side = #RIGHT
  }
}
