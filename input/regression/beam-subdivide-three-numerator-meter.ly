\version "2.25.8"

\header {
  doctitle = "Beam subdivision inside@tie{}3/16 time"

  texidoc = "At position@tie{}1/8, the beam should be subdivided at
the@tie{}1/16 level due to the time signature"
}

\paper {
  indent = 0
  ragged-right = ##t
}


\relative c' {
  \time 3/16
  \set subdivideBeams = ##t
  \set baseMoment = \musicLength 16*3
  \omit Staff.Clef
  \repeat unfold 12 c64
}
