\version "2.25.32"

\header {
  texidoc = "When an ossia staff with @code{\\polymetric \\time} terminates
simultaneously with a change in the reference time signature, there is no
warning."
}

#(ly:set-option 'warning-as-error #t)

\score {
  \layout {}
  \midi {}
  \fixed c' <<
    \time 3/4
    \new Staff {
      \repeat unfold 2 {
        \repeat unfold 6 f8
      }
      \time 4/4
      \repeat unfold 8 f8
    }
    {
      \skip 2.
      \new Staff {
        \context Staff \polymetric \time 6/8
        \repeat unfold 6 f8
      }
    }
  >>
}
