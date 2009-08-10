\version "2.13.4"

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
      \new Staff { \mark \markup \column { T A L L M A R K } c'1 \break
		   \mark \markup \column { T A L L M A R K } c'1 }
      \new Staff { c'1 \break c'1 }
    >>
  }
}
