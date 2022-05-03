\version "2.23.10"

\header {
  texidoc = "A clef is printed at a break, even without a bar line."
}

\paper {
  ragged-right = ##t
}

{ c'2 \break 2 }

{ \cueClef bass c'2 \break 2 }
