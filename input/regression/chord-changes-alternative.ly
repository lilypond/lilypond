\version "2.18.0"

\header {
  texidoc = "Chord change detection in repeat alternatives
happens in relation to the chord active at the beginning of the
first alternative.
"
}

\score {
  <<
    \new ChordNames {
      \set chordChanges = ##t
      \chordmode { \repeat volta 3 { g1 | }
		   \alternative { {c |}  {c4 g c c |} {g2 c |} } } }
    \new Voice {
      \relative c'' {
	\repeat volta 3 { g4 f e d | } 
	\alternative { {c r r r |} {c g c r |} {b g c2 |} } }
      \bar "|." }
  >>
}
