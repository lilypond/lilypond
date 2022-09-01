\version "2.21.0"

\header {
  texidoc = "Context modification via @code{\\with} filters translators
of the wrong type: performers for an @code{Engraver_group} and engravers
for a @code{Performer_group}.  In this test, the
@code{Instrument_name_engraver} is added to a @code{StaffGroup}, but
does not affect midi output, since it is filtered out."
}

\layout {
  \context {
    \StaffGroup
    % Test that it's actually being added to StaffGroup, by cancelling the
    % default \consists.
    \remove Instrument_name_engraver
  }
}

\score {
  \new StaffGroup \with {
    \consists "Instrument_name_engraver"
    instrumentName = "StaffGroup"
  }
  {
    a'1
  }
  \layout { }
  \midi { }
}
