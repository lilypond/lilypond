\version "2.25.8"

\header {
  doctitle = "Beam subdivision inside a compound meter"

  texidoc = "Even under unusual measure lengths, beam
subdivision should not defect"
}

\paper {
  ragged-right = ##t
}

\relative c'' {
  \omit Staff.Clef
  \compoundMeter #'((2 4) (5 32))
  \set baseMoment = \musicLength 32
  \set beatStructure = #'(8 8 5)
  \set subdivideBeams = ##t

  \repeat unfold 21 e32 \break

  \set beatStructure = #'(8 8 2 3)
  \repeat unfold 21 e32 \break
}
