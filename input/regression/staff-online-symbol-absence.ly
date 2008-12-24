\version "2.12.0"
\header {

  texidoc = "Symbols that need on-staffline info (like dots and ties)
  continue to work in absence of a staff-symbol."

}

\new Staff \with {
  \remove Staff_symbol_engraver
}
\relative c' {
  \time 5/8
  f4 ~ f4 
  f4 f4.
}
