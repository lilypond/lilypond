\header {

texidoc = "Instrument names are set with @code{Staff.instrument} and
@code{Staff.instr}. You can enter markup texts to create more funky names,
including alterations. "

}


\version "2.1.7"


textFlat = \markup {\smaller \musicglyph #"accidentals--1"}

\score {
  \notes \new Staff {
    \property Staff.instrument
	= \markup { \column < "Clarinetti" { "in B" \textFlat } > }
    \property Staff.instr
	= \markup { \smaller  { "Cl(B" \textFlat ")" } }

    { c''1 \break c'' }

  }
  \paper { raggedright = ##t }
}


