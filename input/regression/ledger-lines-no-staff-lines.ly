\version "2.25.27"

\header {
  texidoc = "Ledger lines are not printed in the absence of staff lines."
}

\paper { indent = 0 ragged-right = ##t }

\layout {
  \context {
    \Staff
    \consists "Custos_engraver"
    \override Custos.style = #'hufnagel
    %% Use scheme-code for ledgers
    \override StaffSymbol.ledger-positions-function =
      #'(lambda (grob pos)
          (lset-difference
            =
            (ledger-lines::positions-from-ledgered-grob grob pos)
            (ly:grob-property grob 'line-positions)))
    \override StaffSymbol.line-count = 0
  }
}

{ b2 \atLeft -> \break b'' }
