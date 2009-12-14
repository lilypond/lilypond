\version "2.13.10"

\header { texidoc = "If the 'whiteout property of a
grob is set to #t, that part of all objects in lower
layers which falls under the extent of the grob is
whited out.  Here the TimeSignature whites out the
Tie but not the StaffSymbol.
"
}

\relative c' {
  \time 3/4
  \override Staff.StaffSymbol #'layer = #4
  \once \override Tie #'layer = #2
  b'2.~
  \once \override Staff.TimeSignature #'whiteout = ##t
  \once \override Staff.TimeSignature #'layer = #3
  \time 5/4
  b4
}

