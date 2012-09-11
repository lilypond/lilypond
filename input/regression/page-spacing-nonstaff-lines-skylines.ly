\version "2.16.0"

\header {
  texidoc = "Relative indentation between systems is taken into
  account in allowing space for loose lines between systems."
}

\paper {
  ragged-right = ##t
  indent = 10
  short-indent = 00
}

\book {
  \score { <<
    \chords {s1\break c2 c2 }
    \new Staff {\clef bass c,1 | c''2 c2 }
  >> }
}
