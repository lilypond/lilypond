\version "2.25.6"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme "bar check failed at: %s") "3/4")

\header {

texidoc = "skipTypesetting doesn't affect bar checks."

}

\relative {
  c'4
  \set Score.skipTypesetting = ##t
  c4 c4
  |
  c4 c4
  \set Score.skipTypesetting = ##f
  d
}
