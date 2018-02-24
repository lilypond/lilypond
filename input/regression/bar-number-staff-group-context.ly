\version "2.21.0"

\header {
  texidoc="Bar_number_engraver may be moved to staff-group contexts.
Bar numbers should appear in black above the second staff from the
top.  The same numbers should appear in red above the third staff from
the top."
}

\layout {
  indent = 0
  ragged-right = ##t
  \context {
    \Score
    \remove "Bar_number_engraver"
    \override BarNumber.break-visibility = #all-visible
  }
}

<<
  \new Staff {
    R1*2 \break R1*2
  }
  \new ChoirStaff \with {
    \consists "Bar_number_engraver"
    \consists "Staff_collecting_engraver"
  } <<
    \new Staff {
      s1*4
    }
    \new PianoStaff \with {
      \consists "Bar_number_engraver"
      \consists "Staff_collecting_engraver"
      \override BarNumber.color = #red
    } <<
      \new Staff {
        s1*4
      }
      \new Staff {
        s1*4
      }
    >>
  >>
>>
