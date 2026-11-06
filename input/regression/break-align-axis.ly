\version "2.25.0"

\header {
  texidoc = "If @code{BreakAlignment.break-align-axis} returns different
axes for grobs, break-align groups that have no axis in common may share
horizontal space.  Here the key cancellation in the upper staff and the
key signature in the lower staff are not kept apart."
}

\score {
  \new StaffGroup
  <<
    \new Staff {
      \key g \major
      g'1
      \key c \major
      \time 3/4
      c'2.
    }
    \new Staff {
      \key c \major
      d'1
      \key d \major
      \time 3/4
      d'2.
    }
  >>
  \layout {
    \context {
      \Score
      \override BreakAlignment.break-align-axis = ##f
    }
    \context {
      \StaffGroup
      instrumentName = \markup \center-column { global align spacing }
    }
  }
  \layout {
    \context {
      \StaffGroup
      instrumentName = \markup \center-column { local align spacing }
    }
  }
}
