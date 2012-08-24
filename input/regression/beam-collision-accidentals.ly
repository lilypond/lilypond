\version "2.16.0"
\header {
  texidoc = "Beams only check for collisions with in-line accidentals."
}

{
 \set suggestAccidentals = ##t
  a'8[ fis'16 g'16]
  \unset suggestAccidentals
  c'8 [ des'' ]
  r2
}
