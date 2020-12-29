\version "2.23.0"

\header{
  texidoc="Bar numbers on repeat bar lines do not depend on the order
in which @code{Bar_number_engraver} and
@code{Repeat_acknowledge_engraver} run.  The two systems in this test
should be identical."
}

\layout {
  \context {
    \Score
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #all-visible
    \remove Bar_number_engraver
    \remove Repeat_acknowledge_engraver
  }
}

piece = { b2 \repeat volta 2 c'2 \alternative { d'2 e'2 } f'2 | }


\new Score \with {
  \consists Bar_number_engraver
  \consists Repeat_acknowledge_engraver
} {
  \piece
}

\new Score \with {
  \consists Repeat_acknowledge_engraver
  \consists Bar_number_engraver
} {
  \piece
}
