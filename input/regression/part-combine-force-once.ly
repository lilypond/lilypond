
\header {
  texidoc ="Overrides for the part-combiner, affecting only one moment.
  The @code{partCombine...Once} override applies only to one moment, after which the
  old override -- if any -- is in effect again.
"
}

\layout { ragged-right = ##t }

\version "2.21.0"

mI = \relative {
	e'4 e \once \partCombineApart c c |
	\partCombineApart c \once \partCombineChords e e e |
	c \once \partCombineUnisono c c c |
	\partCombineAutomatic \once \partCombineSoloI r2 c4 c |
	\once \partCombineSoloII R1 |
	d'2 \once \partCombineChords d4^"1 chord" d|
}
mII = \relative {
	c'4 \once \partCombineApart c c c |
	c c \once \partCombineAutomatic e e |
	c c c c |
	R1 |
	r2 c4 c |
	b4 b b b |
}

\score {
	\new Staff \partCombine \mI \mII
}
