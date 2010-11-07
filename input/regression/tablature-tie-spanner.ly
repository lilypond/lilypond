\version "2.13.42"

\header {
  texidoc = "
If a slur or a glissando follows a tie, the
corresponding fret number is displayed in parentheses.
"
}

music = {
  c'4 ~ c'4 ( d'2 ) |
  c'4 ~ c'4 \glissando d'2 |
  c'4 ~ c'4 d'2 |
  c'4 \glissando d'2. |
}

\score {
  <<
    \new Staff {
      \new Voice {
        \clef "G_8"
        \music
      }
    }
    \new TabStaff {
      \new TabVoice {
        \music
      }
    }
  >>
}
