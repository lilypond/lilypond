
\header {
  texidoc ="Overrides for the part-combiner. All functions like
  @code{\\partCombineApart} and @code{\\once \partCombineApart} are
  internally implemented using a dedicated @code{partCombineForced}
  context property."
}

\layout { ragged-right = ##t }

\version "2.21.0"

mI = \relative {
	e'4 e c2 |
	\partCombineApart c^"apart" e |
	e e |
	\partCombineChords e'^"chord" e |
	\partCombineAutomatic c c\> |
	\partCombineUnisono c^"unisono" c |
	\partCombineAutomatic c\! c^"V1 longer" |
}
mII = \relative {
	c'4 c c2 |
	c c |
	\partCombineAutomatic e^"auto" e |
	a, c |
	c c' |
	c c |
	c
}

\score {
	\new Staff \partCombine \mI \mII
}
