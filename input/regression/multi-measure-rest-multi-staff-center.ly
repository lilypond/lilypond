\version "2.17.6"

\header {
  texidoc = "Though the default spacing for multi-measure rests
is affected by prefatory matter in other staves, centering can be
restored by overriding @code{spacing-pair}."
}

<<
  \new Staff  {
    \once \override MultiMeasureRest.spacing-pair =
    #'(break-alignment . staff-bar)
    R1
  }
  \new Staff {
    f'1
    \clef bass
  }
>>
