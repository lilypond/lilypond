\version "2.17.7"

\header {

  texidoc="Clef transposition symbols may be parenthesized or
bracketed by using parentheses or brackets in the command string."

}
\score {
  \new Staff {
    \clef "G^(8)" g''1 |
    \clef "bass_[15]" c,,1 |
    \clef "C^(8)" c''1
  }
}
