\header {

texidoc = "Instrument names are set with Staff.instrument and
Staff.instr. You can enter markup texts to create more funky names,
including alterations. "

}


\version "1.7.6"


\score {
  \notes \context Staff = treble {
    \property Staff.instrument
	= \markup { \column << "Clarinetti" { "in B" \smaller \musicglyph #"accidentals--1" } >> }
    \property Staff.instr
	= \markup { \smaller  { "Cl(B" \smaller \musicglyph #"accidentals--1" ")" } }

    { c''1 \break c'' }

  }
  \paper { linewidth= 8.0\cm }
}

%% new-chords-done %%
