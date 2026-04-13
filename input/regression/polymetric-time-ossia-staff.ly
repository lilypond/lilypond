\version "2.25.35"

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
      \*2 { \*6 f8 }
      \time 4/4
      \*8 f8
    }
    {
      \skip 2.
      \new Staff {
        \context Staff \polymetric \time 6/8
        \*6 f8
      }
    }
  >>
}
