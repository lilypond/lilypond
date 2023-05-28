\version "2.25.6"

\header {
  texidoc = "The various kinds of grace music are similar in how they create
  contexts.  In this test, each grace note should create a new staff."
}

{
  R1
  \context Score { \acciaccatura c'8 g'1 }
  \context Score { \appoggiatura c'8 g'1 }
  \context Score { \grace        c'8 g'1 }
  \context Score { \slashedGrace c'8 g'1 }
}
