\version "2.21.0"

\header {
  texidoc="Rehearsal_number_engraver may be moved to staff-group
contexts.  Five marks should appear in black above the second staff
from the top.  The same marks should appear in red above the third
staff from the top."
}

\layout {
  \context {
    \Score
    \remove "Mark_engraver"
  }
}

<<
  \new Staff {
    s1*4
  }
  \new ChoirStaff \with {
    \consists "Mark_engraver"
    \consists "Staff_collecting_engraver"
  } <<
    \new Staff {
      s1*4
    }
    \new StaffGroup \with {
      \consists "Mark_engraver"
      \consists "Staff_collecting_engraver"
      \override RehearsalMark.color = #red
    } <<
      \new Staff \relative {
        s1*4
      }
      \new Staff \relative {
        \mark "Α"
        c'1 \mark "foo"
        c1
        \key cis \major
        \clef alto
        \override Score.RehearsalMark.break-align-symbols = #'(key-signature)
        \mark "on-key"
        cis
        \key ces \major
        \override Score.RehearsalMark.break-align-symbols = #'(clef)
        \clef treble
        \mark "on clef"
        ces
        \mark "Ω"
      }
    >>
  >>
>>
