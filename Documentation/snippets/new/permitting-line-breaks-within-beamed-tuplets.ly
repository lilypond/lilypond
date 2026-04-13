\version "2.25.35"

\header {
  categories = "Contexts and engravers, Rhythms"

  texidoc = "
These artificial examples show how both manual and automatic line
breaks may be permitted within beamed tuplets that can't be
rhythmically split in an exact way.

This feature only works with manually beamed tuplets.
"

  doctitle = "Permitting line breaks within beamed tuplets"
} % begin verbatim


\layout {
  \context {
    \Voice
    % Permit automatic line breaks within tuplets.
    \remove "Forbid_line_break_engraver"
    % Allow beams to be broken at line breaks.
    \override Beam.breakable = ##t
  }
}

\relative c'' {
  <>^"manually forced line break"
  a8
  \*5 { \tuplet 3/2 { c8[ b g16 a] } }
  \tuplet 3/2 { c8[ b \break g16 a] }
  \*5 { \tuplet 3/2 { c8[ b g16 a] } }
  c8 \bar "||"
}

\relative c'' {
  <>^"automatic line break"
  \*28 a16
  \tuplet 11/8 { a16[ b c d e f e d c b a] }
  \*28 a16 \bar "||"
}
