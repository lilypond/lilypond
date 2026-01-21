\version "2.25.24"

\header {
  texidoc = "@code{\\ambitusAfter} should not crash if used in a score that
contains a MIDI output-def.  Issue #6796"
}

\score {
  \new Staff \with {
    \consists Ambitus_engraver
    \ambitusAfter key-signature
  } { c'1 c''1 }
  \midi {}
}
