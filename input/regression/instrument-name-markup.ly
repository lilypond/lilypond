\header {

texidoc = "Instrument names are set with Staff.instrument and
Staff.instr. You can enter markup texts to create more funky names,
including alterations. "

}


\version "1.7.18"


textFlat = \markup {\smaller \musicglyph #"accidentals--1"}
\score {
  \notes \context Staff = treble {
    \property Staff.instrument
	= \markup { \column << "Clarinetti" { "in B" \textFlat } >> }
    \property Staff.instr
	= \markup { \smaller  { "Cl(B" \textFlat ")" } }

    { c''1 \break c'' }

  }
  \paper { linewidth= 8.0\cm }
}


