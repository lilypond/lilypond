\version "2.25.1"

\header {
  texidoc = "Parentheses only cause minimum distances to be set.
They should not cause more space to be allowed for a note when
they do not cause a collision with the previous or the following
note."
}

\paper {
  ragged-right = ##f
}

{ c c c c }

\markup \fill-line { "Notes above and below should be aligned." }

{
  c c c
  % Make sure that the effect would be visible if the
  % test broke.
  \tweak Parentheses.font-size 20
  \parenthesize c
}
