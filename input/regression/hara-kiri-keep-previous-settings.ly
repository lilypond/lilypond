\version "2.14.0"

\header { texidoc =

	  "Inserting the harakiri settings globally into the Staff context should
not erase previous settings to the Staff context.
"

}

\layout {
  ragged-right= ##t
  \context {
    \Staff
    \override StaffSymbol #'line-count = 4
    \consists "Ambitus_engraver"
    \remove "Clef_engraver"
  }
}

% Old \RemoveEmptyStaffContext: Will erase previous settings...
\score {
  <<
    \new Staff \relative c'' {  c4 c c c \break s1 \break c4 c c c \break c c c c}
    \new Staff \relative c'' {  d4 d d d        s1        s1              s1 }
    \new Staff \relative c'' {  e4 e e e        s1        e4 e e e        s1 }
  >>
  \layout {
    \context { \RemoveEmptyStaffContext }
  }
}

% New \RemoveEmptyStaves settings: Preserves previous settings...
\score {
  <<
    \new Staff \relative c'' {  c4 c c c \break s1 \break c4 c c c \break c c c c}
    \new Staff \relative c'' {  d4 d d d        s1        s1              s1 }
    \new Staff \relative c'' {  e4 e e e        s1        e4 e e e        s1 }
  >>
  \layout {
    \context { \Staff \RemoveEmptyStaves }
  }
}
