\header {
   texidoc = "Bar line extent can be customised and the customised value
   must be respected when staff symbol is changed temporarily (e.g. to
   simulate ledger lines of renaissance prints and manuscripts);
   moreover, span bars should not enter the staves."
}


\version "2.17.6"

ledgerUp =
{
  s2
  \stopStaff
  \override Staff.StaffSymbol.line-positions = #'(-4 -2 0 2 4 6)
  \startStaff
  s2
  \noBreak
  s2
  \stopStaff
  \revert Staff.StaffSymbol.line-positions
  \startStaff
  s2
}

ledgerDown =
{
  s2
  \stopStaff
  \override Staff.StaffSymbol.line-positions = #'(-6 -4 -2 0 2 4)
  \startStaff
  s1
  \stopStaff
  \revert Staff.StaffSymbol.line-positions
  \startStaff
  s2
}

\new StaffGroup
<<
  \new Staff \with
  {
    \override BarLine.bar-extent = #'(-1 . 1.5)
  }
  {
    \ledgerUp \ledgerDown
    s1*4
    s1*4
    \ledgerUp \ledgerDown
    \ledgerUp \ledgerDown
    \ledgerUp \ledgerDown
    \ledgerUp \ledgerDown
    s1*4
    s1*4
  }

  \new Staff \with
  {
    \override BarLine.bar-extent = #'(0 . 0)
  }
  {
    s1*4
    s1*4
    \ledgerUp \ledgerDown
    \ledgerUp \ledgerDown
    \ledgerDown \ledgerUp
    s1*4
    s1*4
    \ledgerUp \ledgerDown
    \ledgerUp \ledgerDown
  }

  \new Staff \with
  {
    \override BarLine.bar-extent = #'(-2 . 0)
  }
  {
    s1*4
    \ledgerUp \ledgerDown
    s1*4
    s1*4
    s1*4
    \ledgerUp \ledgerDown
    \ledgerDown \ledgerUp
    \ledgerUp \ledgerDown
    \ledgerDown \ledgerUp
  }
>>
