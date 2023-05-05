\version "2.25.5"

\layout {
  %% Color the Divisio grobs because it is otherwise hard to tell some
  %% of them apart from BarLine grobs.
  \context {
    \Score
    \override Divisio.color = #(universal-color 'blue)
    \consists Measure_spanner_engraver
    \override TextMark.break-visibility = #all-visible
  }
}

\score {
  <<
    %% placing the labels in only one staff is intentional
    \new Staff \with {
      instrumentName = "Staff"
    } << \divisions \labels \music >>

    \new StaffGroup <<
      \new GregorianTranscriptionStaff \with {
        instrumentName = \markup \column { "G.T.Staff" defaults }
      } << \divisions \music >>

      \new GregorianTranscriptionStaff \with {
        instrumentName = \markup \column { "w/ chant-" "quarterbar" }
        caesuraType = #'((breath . chantquarterbar))
      } << \divisions \music >>

    \new GregorianTranscriptionStaff \with {
      instrumentName = \markup \column { preferring Divisio }
      \EnableGregorianDivisiones
      caesuraType = #'((breath . chantquarterbar))
    } << \divisions \music >>
    >>
  >>
}
