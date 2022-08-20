\version "2.23.5"

\header {
  lsrtags = "editorial-annotations, expressive-marks, staff-notation, tweaks-and-overrides"

  texidoc = "
This method prints two 'rehearsal marks' - one above the stave and one
below - by creating two voices, adding both @code{Mark_engraver} and
@code{Mark_tracking_translator} to each voice, and finally placing each
rehearsal mark UP and DOWN in each voice respectively.

This method (as opposed to method 1) is more complex, but allows for
more flexibility, should it be needed to tweak each rehearsal mark
independently of the other.
"

  doctitle = "How to print two rehearsal marks above and below the same barline (method 2)"
}


\score {
  \relative c'
  <<
    \new Staff {
      <<
        \new Voice \with {
          \consists "Mark_engraver"
          \consists "Staff_collecting_engraver"
          \consists "Mark_tracking_translator"

        }
        { c4 d e f
          \mark \markup { \box A }
          c4 d e f
        }
        \new Voice \with {
          \consists "Mark_engraver"
          \consists "Staff_collecting_engraver"
          \override RehearsalMark.direction = #DOWN
          \consists "Mark_tracking_translator"
        }
        { s4 s s s
          \mark \markup { \circle 1 }
          s4 s s s
        }
      >>
    }
  >>
  \layout {
    \context {
      \Score
      \remove "Mark_engraver"
      \remove "Staff_collecting_engraver"
      \remove "Mark_tracking_translator"
    }
  }
}
