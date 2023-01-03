\version "2.24.1"

\header {
  texidoc ="The slur between the stemless notes should begin and end
in the same spaces as the slur between the stemmed notes."
}

\layout { ragged-right = ##t }

music = { a'( d'') }

{
  \new Voice \with { \stemDown } \music
  \new Voice \with { \remove Stem_engraver } \music
}
