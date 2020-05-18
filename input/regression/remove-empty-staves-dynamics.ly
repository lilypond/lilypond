\version "2.21.0"

\layout {
  \context {
    \Score
    \RemoveAllEmptyStaves
  }
}

<<
  \new Staff { c'1 }
  \new Dynamics { s1\p }
>>
