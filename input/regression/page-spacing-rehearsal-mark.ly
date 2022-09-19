\version "2.23.14"

\header {
  texidoc = "The space taken up by rehearsal marks is correctly
accounted for, even though they live in the Score context."
}

#(set-default-paper-size "a6")

\book {
  \paper {
    oddHeaderMarkup = "header"
    ragged-last-bottom = ##f
  }
  \score {
    <<
      \new Staff { \textMark \markup \column { T A L L M A R K } c'1 \break
		   \textMark \markup \column { T A L L M A R K } c'1 }
      \new Staff { c'1 \break c'1 }
    >>
  }
}
