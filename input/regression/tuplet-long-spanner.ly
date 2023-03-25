\version "2.25.3"

\header {
  texidoc = "Overlong tuplet span specifications are reduced
to the actual length."
}

\layout { ragged-right = ##t }

\relative \new Staff \with { subdivideBeams = ##t
			     baseMoment = \musicLength 8
			   }
{
  \tuplet 3/2 4 { g16 a b } c8 d e f2 |
  \set baseMoment = \musicLength 1*1/12
  \tuplet 3/2 4
  { d16 e f g a b  c,16 d e f g a
    \unset baseMoment
    b, c d }
  \tuplet 3/2 { e16 f8 } g4 |
}
