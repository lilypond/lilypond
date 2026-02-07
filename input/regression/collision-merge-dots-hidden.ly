\version "2.25.34"

\header {
  texidoc = "When merging a visible note with a hidden note, the visible dots
remain.  Both notes should have a dot."
}

#(ly:set-option 'warning-as-error #t)

#(set-global-staff-size 30)

\layout {
  %% Hide unnecessary symbols.
  \context {
    \Staff
    \remove Clef_engraver
    \remove Time_signature_engraver
    \remove Staff_symbol_engraver
  }
}

A = b'4.
B = { \hideNotes \A }

\new OneStaff {
  \new Staff << \A \\ \B >>
  \new Staff << \B \\ \A >>
}
