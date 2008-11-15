\version "2.11.64"

\header {
  texidoc = "For a one-page score, ragged-bottom should have the
same effect as ragged-last-bottom."
}

\paper {
  ragged-bottom = ##t
  ragged-last-bottom = ##f
}

\repeat unfold 16 c'4
