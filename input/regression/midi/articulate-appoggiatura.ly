\version "2.27.1"

\header {
  texidoc = "@code{\\articulate} makes appoggiaturas take half the value of the
note following without taking dots into account.  In this case, the starting
note of @code{\\appoggiatura c1024 d2} and of @code{\\appoggiatura e8 f2.} is
changed to a quarter note."
}

#(ly:set-option 'warning-as-error #t)

\include "articulate.ly"

\score {
  \articulate \fixed c' {
    r2 \appoggiatura c1024 d2 |
    r4 \appoggiatura e8 f2. |
    \time 16/1
    r1 \appoggiatura g8 a\maxima... |
  }
  \midi {}
}
