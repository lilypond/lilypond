\version "2.19.21"
\header {

  texidoc = "Symbols that need on-staffline info (like dots and ties)
  continue to work in absence of a staff-symbol."

}

\new Staff \with {
  \remove "Staff_symbol_engraver"
}
\relative {
  \time 5/8
  f'4 ~ 4 
  f4 f4.
}
