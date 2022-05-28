\version "2.23.10"

\header {
  texidoc = "A key signature is printed at a break, even without a
bar line."
}

\paper {
  ragged-right = ##t
}

{ \key c \minor c'2 \break 2 }
