\version "2.23.14"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="By default, @code{GregorianTranscriptionStaff} creates
@code{BarLine} grobs for @code{\\divisio}@dots{} commands, but
@code{\\EnableGregorianDivisiones} makes it create @code{Divisio}
grobs like the ancient-notation staves."
}

\include "divisions-staff-override-music.ily"

\layout {
  %% Color the Divisio grobs because it is otherwise hard to tell some
  %% of them apart from BarLine grobs.
  \context {
    \Score
    \override Divisio.color = #(universal-color 'blue)
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
