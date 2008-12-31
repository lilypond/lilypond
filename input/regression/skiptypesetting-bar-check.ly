\version "2.12.0"

\header {

texidoc = "skipTypesetting doesn't affect bar checks."

}

\relative {
  c4
  \set Score.skipTypesetting = ##t
  c4 c4
  |
  c4 c4
  \set Score.skipTypesetting = ##f
  d
}
