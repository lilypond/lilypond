\version "2.19.21"

\header { texidoc =

	  "Inserting the harakiri settings globally into the Staff context should
not erase previous settings to the Staff context.
"

}

\layout {
  ragged-right= ##t
  \context {
    \Staff
    \override StaffSymbol.line-count = 4
    \consists "Ambitus_engraver"
    \remove "Clef_engraver"
  }
}

\score {
  <<
    \new Staff \relative {  c''4 c c c \break s1 \break c4 c c c \break c c c c}
    \new Staff \relative {  d''4 d d d        s1        s1              s1 }
    \new Staff \relative {  e''4 e e e        s1        e4 e e e        s1 }
  >>
  \layout {
    \context { \Staff \RemoveEmptyStaves }
  }
}
