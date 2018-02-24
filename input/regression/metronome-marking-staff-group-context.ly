\version "2.21.0"

\header {
  texidoc = "Metronome marks are placed correctly if
Metronome_mark_engraver is moved to StaffGroup context.  Metronome
marks should appear above the middle staff (the upper staff of the
group) only."
}

\layout {
  ragged-right = ##t
  \context {
    \Score
    \remove "Metronome_mark_engraver"
  }
}

<<
  \new Staff {
    s1*4
  }
  \new StaffGroup \with {
    \consists "Metronome_mark_engraver"
    \consists "Staff_collecting_engraver"
  } <<
    \new Staff {
      s1*4
    }
    \new Staff \relative c'' {
      \tempo \breve = 100 c1 c1
      \markLengthOn
      \tempo "Allegro" 8.. = 50 c1 \tempo "Adagio" c2 c'
    }
  >>
>>
