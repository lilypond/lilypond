\version "2.25.25"

\header {
  texidoc = "Horizontal @code{Script} may get ledger lines by disabling
@code{no-ledgers}.  The thickness is taken from the relevant properties of
@code{StaffSymbol}.
The @code{length-fraction} may be set for the @code{Script} grob itself."
}

{
  \override Script.padding = 0.7
  \override Script.side-axis = #X
  \override Script.direction = #LEFT
  b2 -\mordent
  b'' -\mordent
  b'1 -\mordent
  b2 \tweak no-ledgers ##f -\mordent
  b'' \tweak no-ledgers ##f -\mordent

  \override Script.no-ledgers = ##f
  b2 -\mordent
  b'' -\mordent
  b'1 -\mordent
  \stopStaff
  \startStaff
  \override Staff.StaffSymbol.thickness = 4
  b1 -\mordent
  \stopStaff
  \startStaff
  \revert Staff.StaffSymbol.thickness
  \override Script.length-fraction = 1.5
  b1 -\mordent
  \section
}
