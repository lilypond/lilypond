\version "2.16.0"
\header {

  texidoc = "There are both long and short instrument names.
Engraving instrument names should not be confused by the
multimeasure rests. "

}

\layout {
  ragged-right = ##t
}


\context Staff <<
  \set Staff.instrumentName = "instrument"
  \set Staff.shortInstrumentName = "instr"
  {c''1 \break R1 }
>>
