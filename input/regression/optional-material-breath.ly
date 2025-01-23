\version "2.25.25"

\header {
  texidoc = "Breath marks are placed inside brackets for optional material.
Clefs, key signatures, and time signatures are placed outside."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  ragged-right = ##t
}

\fixed c' {
  c1 \break

  \startOptionalMaterial
  \clef "alto"
  \key f \major
  \time 2/2
  d4 e f g \breathe
  \stopOptionalMaterial
  \clef "bass"
  \key g \major
  \time 9/8
  \break

  c1
}
