\version "2.27.6"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (ly:translate-cpp-warning-scheme
		     "cannot fit the music into %d systems; using %d")
  5 8)

\paper {
  page-breaking = #ly:page-turn-breaking
  system-count = 5
}

\new PianoStaff <<
  \new Staff { \repeat unfold 160 { c'8 c'' } }
  \new Staff { \repeat unfold 160 { c8 c' } }
>>
