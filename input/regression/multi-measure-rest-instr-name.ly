\version "2.7.13"
\header {

  texidoc = "There are both long and short instrument names.
Engraving instrument names should not be confused by the
multimeasure rests. "

}

\layout {
  raggedright = ##t
  \context {
    \Staff
    \consists Instrument_name_engraver

  }
}


\context Staff <<
  \set Staff.instrument = "instrument"
  \set Staff.instr = "instr"
  {c''1 \break R1 }
>>
