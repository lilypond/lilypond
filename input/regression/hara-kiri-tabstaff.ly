\version "2.16.0"

\header {
  texidoc =

      " Hara-kiri staves are suppressed if they are empty.  This
example really contains three tab staves, but as it progresses, empty ones
are removed: this example has three staves, but some of them
disappear: note how the 2nd line only has the bar number 2. (That the
bar number is printed might be considered a bug, however, the scenario
of all staves disappearing does not happen in practice.)"

}

\layout {
  ragged-right= ##t
  \context {
    \TabStaff
    \RemoveEmptyStaves
  }
}


\new GrandStaff <<
  \new TabStaff {
    c4 c c c \break
    s1 \break
    c4 c c c \break
    c c c c
  }
  \new TabStaff {
    d4 d d d
    s1
    s1
    s1
  }
  \new TabStaff {
    e4 e e e
    s1
    e4 e e e
    s1
  }
>>

