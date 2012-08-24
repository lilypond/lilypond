\version "2.16.0"

\header {
  texidoc = "Page breaks are allowed by default at the end of the score,
but the user can override them.  There should be one line on the first
page and two (colliding) lines on the second page."
}

\paper {
  paper-height = 4\cm
}

\book {
{ c'4 }

{ c'4 } \noPageBreak

{ c'4 }
}
