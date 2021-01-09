\version "2.23.0"

\header {
  texidoc = "In a piece with a single, magnified staff, the presence
of a rehearsal mark does not affect spacing from the left edge.  The
clefs in the two systems should appear the same distance from the left
edge."
}

\paper {
  indent = 0
  ragged-right = ##t
}

\new Score \with {
  \remove "Bar_number_engraver" % to eliminate any potential effect
} <<
  \new Staff \with {
    \magnifyStaff 2
    \remove "Time_signature_engraver" % to simplify the output
  } {
    s1 \break \mark \default s1
  }
>>
