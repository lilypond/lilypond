\version "2.16.0"
\header {
  texidoc =

      " Hara-kiri staves are suppressed if they are empty.  This
example really contains three drum staves, but as it progresses, empty ones
are removed: this example has three staves, but some of them
disappear: note how the 2nd line only has the bar number 2. (That the
bar number is printed might be considered a bug, however, the scenario
of all staves disappearing does not happen in practice.)

Any staff brackets and braces are removed, both in the single staff
and no staff case.
"

}

\layout {
  ragged-right = ##t
  \context {
    \DrumStaff
    \RemoveEmptyStaves
  }
}

\transpose c c''
\context StaffGroup <<
  \new DrumStaff
  \drummode {
    sn4 sn sn sn \break
    s1 \break
    sn4 sn sn sn \break
    sn sn sn sn
  }
  \new DrumStaff
  \drummode {
    hh4 hh hh hh
    s1
    s1
    s1
  }
  \new DrumStaff
  \drummode {
    bd4 bd bd bd
    s1
    bd4 bd bd bd
    s1
  }
>>
