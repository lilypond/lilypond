\version "2.14.0"
#(ly:set-option 'warning-as-error #f)

\header {

texidoc = "skipTypesetting doesn't affect bar checks."

}

\relative c' {
  c4
  \set Score.skipTypesetting = ##t
  c4 c4
  |
  c4 c4
  \set Score.skipTypesetting = ##f
  d
}
