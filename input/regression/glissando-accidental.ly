\header {
  texidoc = "Glissandi stop before hitting accidentals.
Chord glissandi stop at the same horizontal position and
have the same slope, they do not cross."
}
\version "2.21.0"

\relative {
  a'1\glissando cis\glissando as
  <f, a>\glissando <f'' a>\glissando
  <fis,, a>\glissando <fis'' a>\glissando
  <fis,, ais>\glissando <fis'' ais>\glissando
  <f,, ais>\glissando <f'' ais>
}
