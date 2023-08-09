\version "2.25.8"

\header {
  doctitle = "Beam subdivision of a triplet, sextuplet, dodecuplet and
qattuorvigintuplet"

  texidoc = "Even though their tuplet ratios are the same when
simplified each additional power of 2 in the tuplet numerator
subdivides an additional half"
}


\paper {
  indent = 0
  ragged-right = ##t
}

notes = {
  \repeat unfold 24 c64
}

\relative c'' {
  \time 1/4
  \set subdivideBeams = ##t
  \override TupletNumber.text = #tuplet-number::calc-fraction-text
  \omit Staff.Clef


  \tuplet 3/2 { \notes } \break
  \tuplet 6/4 { \notes } \break
  \tuplet 12/8 { \notes } \break
  \tuplet 24/16 { \notes } \break
}
