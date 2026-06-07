\version "2.27.2"

\header {
  texidoc = "@code{\\partial} can be used within @code{\\scaleDurations …
\\polymetric \\time}.  Its duration is scaled like any other duration."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  indent = 0
}

\fixed c' <<
  \new Staff {
    \time 3/2
    \partial 2.
    f2. |
    \*12 f8 |
  }
  \new Staff {
    \scaleDurations 3/2 {
      \context Staff \polymetric \time 4/4
      \partial 2
      c2 |
      \*8 c8 |
    }
  }
>>
