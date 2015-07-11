\version "2.19.24"

\header {
  texidoc = "@code{\\tweak} commands can be used to effect overrides
when given a symbol list as argument.  Such overrides can be the
target of another tweak, with the tweaks accumulating.  This example
should show the starting chord with blue, cross-styled note heads
and a red stem."
}

\layout { ragged-right = ##t }

{
  \once \tweak Stem.color #red
  \tweak color #blue
  \tweak style #'cross NoteHead
  <c' e'>2 c'2
}
