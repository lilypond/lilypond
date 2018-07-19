\version "2.21.0"

\header {
  texidoc = "Ligature brackets should align to visible or transparent
stems only. For stemless notes they should span the whole note width."
}

\relative {
  \[ a'1 b \]
  \[ a4 b \]
  \hide Stem
  \[ a b \]
  \omit Stem % stencil = ##f
  \[ a4 b \]
  \override Stem.stencil = #'()
  \[ a b \]
}