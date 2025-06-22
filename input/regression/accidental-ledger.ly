\version "2.19.21"

\header {
  texidoc = "Ledger lines are shortened when there are accidentals.
Depending on the accidental, more than a single staff line gets shortened.
This happens only if the accidental is horizontally close to the head."
}

wholeA = \relative c''' {
  gisis1 gisih gis gih g geh ges geseh geses gis? \break }
wholeB = \relative c''' {
  aisis1 aisih ais aih a aih as aeseh ases ais? \break }

quartersA = \relative c''' {
  \stemUp gisis!4 \stemDown gisis!
  \stemUp gisih! \stemDown gisih!
  \stemUp gis! \stemDown gis!
  \stemUp gih! \stemDown gih!
  \stemUp g! \stemDown g!
  \stemUp geh! \stemDown geh!
  \stemUp ges! \stemDown ges!
  \stemUp geseh! \stemDown geseh!
  \stemUp geses! \stemDown geses!
  \stemUp gis? \stemDown gis? \break }
quartersB = \relative c''' {
  \stemUp aisis!4 \stemDown aisis!
  \stemUp aisih! \stemDown aisih!
  \stemUp ais! \stemDown ais!
  \stemUp aih! \stemDown aih!
  \stemUp a! \stemDown a!
  \stemUp aeh! \stemDown aeh!
  \stemUp aes! \stemDown aes!
  \stemUp aeseh! \stemDown aeseh!
  \stemUp aeses! \stemDown aeses!
  \stemUp ais? \stemDown ais? \break }

{
  \omit Staff.Clef
  \omit Staff.TimeSignature
  \override Staff.StaffSymbol.line-count = #1
  \override Staff.StaffSymbol.ledger-extra = 5

  \set Staff.extraNatural = ##f
  \cadenzaOn

  \override Staff.StaffSymbol.ledger-positions = #'(0 1)
  \wholeA

  \stopStaff\startStaff
  \revert Staff.StaffSymbol.ledger-positions
  \wholeA
  \wholeB

  \stopStaff\startStaff
  \override Staff.StaffSymbol.ledger-positions = #'(0 1)
  \magnifyMusic 0.63 \wholeA

  \stopStaff\startStaff
  \revert Staff.StaffSymbol.ledger-positions
  \magnifyMusic 0.63 \wholeA
  \magnifyMusic 0.63 \wholeB

  \stopStaff\startStaff
  \override Staff.StaffSymbol.ledger-positions = #'(0 1)
  \quartersA

  \stopStaff\startStaff
  \revert Staff.StaffSymbol.ledger-positions
  \quartersA
  \quartersB

  \stopStaff\startStaff
  \override Staff.StaffSymbol.ledger-positions = #'(0 1)
  \magnifyMusic 0.63 \quartersA

  \stopStaff\startStaff
  \revert Staff.StaffSymbol.ledger-positions
  \magnifyMusic 0.63 \quartersA
  \magnifyMusic 0.63 \quartersB
}

\paper {
  indent = 0
}
