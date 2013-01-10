
\header {
  texidoc ="Overrides for the part-combiner. All functions like
  @code{\\partcombineApart} and @code{\\partcombineApartOnce} are internally implemented
  using a dedicated @code{PartCombineForceEvent}.
"
}

\layout { ragged-right = ##t }

\version "2.16.0"

mI = \relative c' {
	e4 e c2 |
	\partcombineApart c^"apart" e |
	e e |
	\partcombineChords e'^"chord" e |
	\partcombineAutomatic c c\> |
	\partcombineUnisono c^"unisono" c |
	\partcombineAutomatic c\! c^"V1 longer" |
}
mII = \relative c' {
	c4 c c2 |
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
