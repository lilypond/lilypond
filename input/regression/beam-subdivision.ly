\header {

  texidoc = "Beam count at subdivisions should match the location
of the current subdivision."
  }

\version "2.25.3"

\layout {
  ragged-right = ##t
}

{
  \set subdivideBeams = ##t
  \set baseMoment = \musicLength 4
  c''32[^\markup{"baseMoment 1/4"} c'' c'' c'' c'' c'' c'' c'']
  \set baseMoment = \musicLength 8
  c''32[^\markup{"baseMoment 1/8"} c'' c'' c'' c'' c'' c'' c'']
  \set baseMoment = \musicLength 16
  c''32^\markup{"baseMoment 1/16"}[ c'' c'' c'' c'' c'' c'' c'']
  \set baseMoment = \musicLength 32
  c''64^\markup{"baseMoment 1/32"}[ \repeat unfold 14 {c''64} c''64]
}

