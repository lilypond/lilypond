\version "2.25.8"


\header {

  texidoc = "Beam count at subdivisions should match the location
of the current subdivision."
}


\layout {
  indent = 1
  ragged-right = ##t
}

\relative c'' {
  \set subdivideBeams = ##t
  \omit Staff.Clef
  \omit Staff.TimeSignature

  \set minimumBeamSubdivisionInterval = \musicLength 4
  c32[^\markup{"minimumBeamSubdivisionInterval 1/4"} c c c c c c c]
  \break

  \set minimumBeamSubdivisionInterval = \musicLength 8
  c32[^\markup{"minimumBeamSubdivisionInterval 1/8"} c c c c c c c]
  \break

  \set minimumBeamSubdivisionInterval = \musicLength 16
  c32^\markup{"minimumBeamSubdivisionInterval 1/16"}[ c c c c c c c]
  \break

  \set minimumBeamSubdivisionInterval = \musicLength 32
  c64^\markup{"minimumBeamSubdivisionInterval 1/32"}[ \repeat unfold 14 {c64} c64]
  \break

  \unset minimumBeamSubdivisionInterval

  \set maximumBeamSubdivisionInterval = \musicLength 4
  c32[^\markup{"maximumBeamSubdivisionInterval 1/4"} c c c c c c c]
  \break

  \set maximumBeamSubdivisionInterval = \musicLength 8
  c32[^\markup{"maximumBeamSubdivisionInterval 1/8"} c c c c c c c]
  \break

  \set maximumBeamSubdivisionInterval = \musicLength 16
  c32^\markup{"maximumBeamSubdivisionInterval 1/16"}[ c c c c c c c]
  \break

  \set maximumBeamSubdivisionInterval = \musicLength 32
  c64^\markup{"maximumBeamSubdivisionInterval 1/32"}[ \repeat unfold 14 {c64} c64]
  \break
}

