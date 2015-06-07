\header {
  texidoc = "Notes are entered using absolute octaves,
octaves relative to the previous note, or relative to a fixed octave."
  }
\version "2.19.21"

\new Staff {
  \relative { c''4 g \absolute { c'' } e' \fixed c'' { g1 }}
  \fixed c'' { c4 \fixed c' { g } c e \relative { g''1 } }
  \clef bass \relative { c4 g c e g1 }
  \fixed c { c4 g, c e g1 }
}
