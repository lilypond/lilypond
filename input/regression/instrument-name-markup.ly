\header {

  texidoc = "Instrument names are set with @code{Staff.instrument} and
@code{Staff.instr}. You can enter markup texts to create more funky names,
including alterations. "

}


\layout { ragged-right = ##t }

\version "2.16.0"

textFlat = \markup {\smaller \flat}

\new Staff {
  \set Staff.instrumentName = \markup { \column { "Clarinetti" \line { "in B" \textFlat } } }
  \set Staff.shortInstrumentName = \markup { \smaller  { "Cl(B" \textFlat ")" } }

  { c''1 \break c'' }
}




