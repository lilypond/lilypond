\version "2.23.14"

\header {
  lsrtags = "text"

  texidoc = "
Although marks are normally only printed above the topmost staff,
they may also be printed on every staff.
"

  doctitle = "Printing marks on every staff"
}


\score {
  <<
    \new Staff { \mark \default c''1 \textMark "molto" c'' }
    \new Staff { \mark \default c'1 \textMark "molto" c' }
  >>
  \layout {
    \context {
      \Score
      \remove Mark_engraver
      \remove Text_mark_engraver
      \remove Staff_collecting_engraver
    }
    \context {
      \Staff
      \consists Mark_engraver
      \consists Text_mark_engraver
      \consists Staff_collecting_engraver
    }
  }
}
