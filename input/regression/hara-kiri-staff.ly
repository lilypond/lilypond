\version "2.16.0"

\header { texidoc =

	  " Hara-kiri staves kill themselves if they are empty.  This
example really contains three staves, but as they progress, empty ones
are removed: this example has three staves, but some of them
disappear: note how the 2nd line only has the bar number 2. (That the
bar number is printed might be considered a bug, however, the scenario
of all staves disappearing does not happen in practice.)

Any staff brackets and braces are removed, both in the single staff
and no staff case.
"
	  
}

\layout {
  ragged-right= ##t
  \context {
    \Staff
    \RemoveEmptyStaves
  }
}

\transpose c c''
\context GrandStaff <<
  \new Staff {  c4 c c c \break s1 \break c4 c c c \break c c c c}
  \new Staff {  d4 d d d        s1        s1              s1 }
  \new Staff {  e4 e e e        s1        e4 e e e        s1 }
>>



