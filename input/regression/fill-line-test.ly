\version "2.16.0"

\header {
  texidoc = "The @code{\\fill-line} markup command should align texts in
columns.  For example, the characters in the center should form one column.

Also test behaviour if entries are too long; there shouldn't be any
collisions."
}

#(set-default-paper-size "a6landscape")

\paper {
  bookTitleMarkup = \markup {
    \column {
      \fill-line { 1 }
      \fill-line { 1 2 }
      \fill-line { 1 2 3 }
      \fill-line { 1 2 3 4 }
      \fill-line { 1 2 3 4 5 }
      \fill-line { 1 2 3 4 5 6 }
      \fill-line { 1 2 3 4 5 6 7 }
      \fill-line { 1 2 3 4 5 6 7 8 }
      \fill-line { 1 2 3 4 5 6 7 8 9 }
      \fill-line { aaa bbbb ccccc dddd eee ffff ggggg hhhh iii }

      \vspace #1

      \fill-line { 11111111 2 3 4 5 6 7 8 9 }
      \fill-line { 1 2 3 4 5 6 7 8 99999999 }
      \fill-line { 1 2 3 44444444 55555555 66666666 7 8 9 }
    }
  }

  tagline = ##f
}

\book {
  \score {
    \new Staff \relative c'' {
      \repeat unfold 4 c1
    }
  }
}
