\version "2.25.25"

\header {
  texidoc = "Horizontal @code{Script} may get ledger lines by disabling
@code{no-ledgers}.  The thickness is taken from the relevant properties of
@code{StaffSymbol}.
The @code{length-fraction} may be set for the @code{Script} grob itself.
Works with unusual settings as well."
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
  \once \override Script.length-fraction = 1.5
  b1 -\mordent
  \section
  \stopStaff
  \startStaff
  \break
  \once \override Staff.BarLine.bar-extent = #'(-2 . 2)
  \override Script.extra-spacing-width = #'(-2 . 2)
  \override Staff.StaffSymbol.line-positions = #'(-11 11)
  \override Staff.StaffSymbol.ledger-positions = #'(0 (2 3 4) 6)

  g4\mordent
  c'->
  e'\prall
  g'\segno

  \once \override Script.ledger-positions = #'(4 6 8 9 10)
  \once \override Script.staff-position =
    #(horizontal-script::calc-staff-position 1.5)
  c''->

  \once \override Script.staff-position =
    #(horizontal-script::calc-staff-position -4)
  e''\turn

  \once \override NoteHead.ledger-positions = #'(6 8 9 10)
  g''->

  \once \override Script.ledger-positions = #'(3  13)
  c'''\segno

  \fine
}
