\version "2.16.0"

\header {
  texidoc = "When ragged-right is specifically disabled, a score with only one
line will not be printed as ragged."
}

\paper {
  ragged-right = ##f
}

{ a b c d }
