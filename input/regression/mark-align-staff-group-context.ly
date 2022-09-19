\version "2.23.14"

\header {
  texidoc="@code{Text_mark_engraver} may be moved to staff-group
contexts.  Five marks should appear in black above the second staff
from the top.  The same marks should appear in red above the third
staff from the top."
}

\layout {
  \context {
    \Score
    \remove Text_mark_engraver
  }
}

<<
  \new Staff {
    s1*4
  }
  \new ChoirStaff \with {
    \consists Text_mark_engraver
    \consists Staff_collecting_engraver
  } <<
    \new Staff {
      s1*4
    }
    \new StaffGroup \with {
      \consists Text_mark_engraver
      \consists Staff_collecting_engraver
      \override TextMark.color = #red
    } <<
      \new Staff \relative {
        s1*4
      }
      \new Staff \relative {
        \textMark "Α"
        c'1 \textMark "foo"
        c1
        \key cis \major
        \clef alto
        \override Score.TextMark.break-align-symbols = #'(key-signature)
        \textMark "on-key"
        cis
        \key ces \major
        \override Score.TextMark.break-align-symbols = #'(clef)
        \clef treble
        \textMark "on clef"
        ces
        \textEndMark "Ω"
      }
    >>
  >>
>>
