\version "2.14.0"

\header {
  lsrtags = "staff-notation, fretted-strings"

  texidoc = "
Tablature can be formatted using letters instead of numbers.

"
  doctitle = "Letter tablature formatting"
}

music = \relative c {
  c4 d e f
  g4 a b c
  d4 e f g
}

<<
  \new Staff {
    \clef "G_8"
    \music
  }
  \new TabStaff \with {
    tablatureFormat = #fret-letter-tablature-format
  }
  {
    \music
  }
>>
