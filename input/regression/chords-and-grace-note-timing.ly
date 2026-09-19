\version "2.27.5"

\header {
  texidoc = "Noteless chords (zero duration) can move before grace notes in adjacent contexts."
}

\layout { ragged-right = ##t }

\score {
    <<
      \new Staff { \appoggiatura c'4 c''1 }
      \new Staff { <>^"^grace" c'1_\markup \typewriter "<>^\"^grace\" c'1" }
    >>
}

\score {
  <<
    \new Staff { \appoggiatura c'4 c''1 }
    \new Staff { <c'>1^"^nongrace"_\markup \typewriter "<c'>1^\"^nongrace\"" }
  >>
}
