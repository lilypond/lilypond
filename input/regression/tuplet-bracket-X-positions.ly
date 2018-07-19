\version "2.21.0"

\header {
  texidoc = "Tuplet brackets should align to visible or transparent
stems only. For stemless notes or rests they should span the whole
note width."
}

\relative {
  \override TupletBracket.bracket-visibility = ##t
  \override TupletBracket.shorten-pair = #'(0 . 0)

  \tuplet 3/2 1 { g'1 g2 g g1 }

  \tuplet 3/2 4 { a8[ a r] r[ a a] }
  \override Stem.stemlet-length = #1
  \tuplet 3/2 4 { a[ a r] r[ a a] }
}