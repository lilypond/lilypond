\version "2.19.62"

\header {
  lsrtags = "pitches, specific-notation"

  texidoc = "
Aiken head white notes get harder to read at smaller staff sizes,
especially with ledger lines. Losing interior white space makes them
appear as quarter notes.
"
  doctitle = "Aiken head thin variant noteheads"
}

\score {
  {
    \aikenHeads
    c''2 a' c' a

    % Switch to thin-variant noteheads
    \set shapeNoteStyles = ##(doThin reThin miThin
                              faThin sol laThin tiThin)
    c'' a' c' a
  }
}
% END EXAMPLE
