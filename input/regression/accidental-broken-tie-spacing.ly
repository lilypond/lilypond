\version "2.12.0"

\header {
  texidoc = "When a tie is broken, the spacing engine must consider the
accidental after the line break, to prevent a collision from occurring."
}

{ \key g \major gis''1~ \break gis''4
\repeat unfold 43 {d4 \noBreak} }


