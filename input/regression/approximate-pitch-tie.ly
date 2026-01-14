\version "2.25.33"

\header {
  texidoc = "When an approximate pitch is tied to an isolated duration (in this
case, @code{~@tie{}2}), the note following the tie is also approximate."
}

#(ly:set-option 'warning-as-error #t)

\new Staff \with {
  %% omit irrelevant stuff to improve automatic difference detection
  \remove Bar_engraver
  \remove Clef_engraver
  \remove Time_signature_engraver
  \remove Staff_symbol_engraver
} {
  \approximatePitch g'2 ~ 2
  \approximatePitch d''2 ~ 2
}
