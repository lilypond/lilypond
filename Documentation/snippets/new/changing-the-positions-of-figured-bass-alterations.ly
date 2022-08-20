\version "2.23.11"

\header {
  lsrtags = "chords"

  texidoc = "
Accidentals and plus signs can appear before or after the numbers,
depending on the @code{figuredBassAlterationDirection} and
@code{figuredBassPlusDirection} properties.

If plus signs appear after the number, specially designed glyphs are
provided for some figures.
"

  doctitle = "Changing the positions of figured bass alterations"
}


\figures {
  <5\+> <5+ 4\+> <6 4- 2\+> r
  \set figuredBassAlterationDirection = #RIGHT
  <5\+> <5+ 4\+> <6 4- 2\+> r
  \set figuredBassPlusDirection = #RIGHT
  <5\+> <5+ 4\+> <6 4- 2\+> r
  \set figuredBassAlterationDirection = #LEFT
  <5\+> <5+ 4\+> <6 4- 2\+> r
}
