\version "2.16.0"
#(ly:set-option 'warning-as-error #f)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "barcheck failed at: %s") "3/4")

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
