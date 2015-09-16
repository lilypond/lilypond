
\header {
  texidoc ="Overrides for the part-combiner. All functions like
  @code{\\partcombineApart} and @code{\\once \partcombineApart} are
  internally implemented using a dedicated @code{partCombineForced}
  context property."
}

\layout { ragged-right = ##t }

\version "2.19.29"

mI = \relative {
	e'4 e c2 |
	\partcombineApart c^"apart" e |
	e e |
	\partcombineChords e'^"chord" e |
	\partcombineAutomatic c c\> |
	\partcombineUnisono c^"unisono" c |
	\partcombineAutomatic c\! c^"V1 longer" |
}
mII = \relative {
	c'4 c c2 |
	c c |
	\partcombineAutomatic e^"auto" e |
	a, c |
	c c' |
	c c |
	c
}

\score {
	\new Staff \partcombine \mI \mII
}
