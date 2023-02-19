\version "2.27.6"

\header {
  texidoc = "The engravers for tempo spanners and marks can be moved to
@code{Staff} level."
}

#(ly:set-option 'debug-skylines #t)
#(ly:set-option 'warning-as-error #t)

\layout { ragged-right = ##t }

\score {
  \new StaffGroup
  <<
    \new Staff { R1*4 }
    \new Staff { \tempoMark "start" \tempoSpan b1 1 1 \tempoMark "stop" 1 }
  >>
    \layout {
      \context {
        \StaffGroup
        instrumentName = \markup { \typewriter Score level }
      }
  }
  \layout {
    \context {
      \Score
      \remove "Tempo_mark_engraver"
      \remove "Tempo_align_engraver"
      \remove "Tempo_spanner_engraver"
      \remove "Staff_collecting_engraver"
      \remove "Metronome_mark_engraver"
    }
    \context {
      \StaffGroup
      instrumentName = \markup { \typewriter Staff level }
    }
    \context {
      \Staff
      \consists "Tempo_mark_engraver"
      \consists "Tempo_spanner_engraver"
      \consists "Tempo_align_engraver"
      \consists "Staff_collecting_engraver"
      \consists "Metronome_mark_engraver"
    }
  }
}
