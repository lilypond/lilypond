\version "2.19.25"

\header {
  texidoc = "Overrides can be the target of a @code{\\tweak}, with the
tweaks accumulating as override.  The main application is for stacking
commands implemented in terms of @code{\\propertyTweak}.  This example
should show the starting chord with blue, cross-styled note heads and
a red stem."
}

\layout { ragged-right = ##t }

{
  \once \tweak Stem.color #red
  \tweak color #blue
  \override NoteHead.style = #'cross
  <c' e'>2 c'2
}
