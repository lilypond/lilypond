
\header {

texidoc = "3 ways to customize ledger line positions."

}

\version "2.27.5"
\paper { ragged-right = ##t }

\relative {
  \override Staff.StaffSymbol.ledger-positions = #'(7 10)
  f''4 g a b c d e f g a b c
}

\relative {
  \override Staff.StaffSymbol.ledger-positions-function =
    #(ly:make-no-callback (lambda (staff-symbol-grob pos) (list pos)))
  c'''4 d e f
}

\relative {
  c'''4
  \once \override NoteHead.ledger-positions = #'(8 10 12 14)
  d e f
}
