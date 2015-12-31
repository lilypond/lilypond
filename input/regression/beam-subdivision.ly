\header {

  texidoc = "Beam count at subdivisions should match the location
of the current subdivision."
  }

\version "2.19.19"

\layout {
  ragged-right = ##t
}

{
  \set subdivideBeams = ##t
  \set baseMoment = #(ly:make-moment 1 4)
  c''32[^\markup{"baseMoment 1/4"} c'' c'' c'' c'' c'' c'' c'']
  \set baseMoment = #(ly:make-moment 1 8)
  c''32[^\markup{"baseMoment 1/8"} c'' c'' c'' c'' c'' c'' c'']
  \set baseMoment = #(ly:make-moment 1 16)
  c''32^\markup{"baseMoment 1/16"}[ c'' c'' c'' c'' c'' c'' c'']
  \set baseMoment = #(ly:make-moment 1 32)
  c''64^\markup{"baseMoment 1/32"}[ \repeat unfold 14 {c''64} c''64]
}

