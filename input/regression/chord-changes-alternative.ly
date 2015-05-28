\version "2.19.21"

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
      \relative {
	\repeat volta 3 { g'4 f e d | } 
	\alternative { {c r r r |} {c g c r |} {b g c2 |} } }
      \bar "|." }
  >>
}
