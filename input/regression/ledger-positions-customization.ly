
\header {

texidoc = "3 ways to customize ledger line positions."

}

\version "2.19.41"
\paper { ragged-right = ##t }

\relative {
  \override Staff.StaffSymbol.ledger-positions = #'(7 10)
  f''4 g a b c d e f g a b c
}

\relative {
  % note: the scheme procedure (lambda expression) is quoted
  \override Staff.StaffSymbol.ledger-positions-function =
    #'(lambda (staff-symbol-grob pos) (list pos))
  c'''4 d e f
}

\relative {
  c'''4
  \once \override NoteHead.ledger-positions = #'(8 10 12 14)
  d e f
}
