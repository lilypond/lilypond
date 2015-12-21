\version "2.19.32"

\header { texidoc = "If the 'whiteout property of a
grob is set to a number or #t, that part of all objects
in lower layers which falls under the extent of
the grob's whiteout area is whited out.  Here the
TimeSignature whites out the Tie but not the StaffSymbol.
"
}

\relative {
  \time 3/4
  \override Staff.StaffSymbol.layer = 4
  \once \override Tie.layer = 2
  b'2.~
  \once \override Staff.TimeSignature.whiteout = ##t
  \once \override Staff.TimeSignature.layer = 3
  \time 5/4
  b4
}

\relative c' {
  \time 3/4
  \override Staff.StaffSymbol.layer = 4
  \once \override Tie.layer = 2
  b'2.~
  \once \override Staff.TimeSignature.whiteout-style = #'rounded-box
  \once \override Staff.TimeSignature.whiteout = 3
  \once \override Staff.TimeSignature.layer = 3
  \time 5/4
  b4
}

\relative c' {
  \time 3/4
  \override Staff.StaffSymbol.layer = 4
  \once \override Tie.layer = 2
  b'2.~
  \once \override Staff.TimeSignature.whiteout-style = #'outline
  \once \override Staff.TimeSignature.whiteout = 3
  \once \override Staff.TimeSignature.layer = 3
  \time 5/4
  b4
}
