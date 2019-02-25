\version "2.21.0"
\header {
  texidoc="
Instrument names can also be attached to staff groups.
"
}

\paper {
  left-margin = 3\cm
}
\layout {
  ragged-right = ##t
}


\new StaffGroup \with { instrumentName = "StaffGroup" } <<
  \new PianoStaff \with { instrumentName = "PianoStaff" } <<
    \new Staff \with { instrumentName = "Right" } { c''4 }
    \new Staff \with { instrumentName = "Left" } { \clef bass c4 }
  >>

  \new ChoirStaff \with { instrumentName = "ChoirStaff" } <<
    \new Staff { c''4 }
    \new Staff { c''4 }
    \new Staff { c''4 }
  >>
  \new GrandStaff \with { instrumentName = "GrandStaff" } <<
    \new Staff \with { instrumentName = "I" } { c''4 }
    \new Staff \with { instrumentName = "II" } { \clef bass c4 }
  >>
  % Nested groups should not inherit the instrument name from the parent group
  \new StaffGroup \with { instrumentName = "nested group" } <<
    \new Staff { c''4 }
    \new StaffGroup <<
      \new Staff { c''4 }
      \new Staff { c''4 }
    >>
  >>
>>
