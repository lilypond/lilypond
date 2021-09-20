\version "2.23.4"

\header {
  texidoc = "Balloons work on spanners that have no pure height."
}

\new Staff \with {
  \consists "Balloon_engraver"
} {
  \balloonGrobText Beam #'(0 . -2) "beam"
  % Doesn't work well (yet), but should not crash.
  \balloonGrobText LedgerLineSpanner #'(1 . 0.5) "ledger line"
  a''16[ 16]
}
