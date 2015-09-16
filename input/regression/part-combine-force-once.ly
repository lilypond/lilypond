
\header {
  texidoc ="Overrides for the part-combiner, affecting only one moment.
  The @code{partcombine...Once} override applies only to one moment, after which the
  old override -- if any -- is in effect again.
"
}

\layout { ragged-right = ##t }

\version "2.19.29"

mI = \relative {
	e'4 e \once \partcombineApart c c |
	\partcombineApart c \once \partcombineChords e e e |
	c \once \partcombineUnisono c c c |
	\partcombineAutomatic \once \partcombineSoloI r2 c4 c |
	\once \partcombineSoloII R1 |
	d'2 \once \partcombineChords d4^"1 chord" d|
}
mII = \relative {
	c'4 \once \partcombineApart c c c |
	c c \once \partcombineAutomatic e e |
	c c c c |
	R1 |
	r2 c4 c |
	b4 b b b |
}

\score {
	\new Staff \partcombine \mI \mII
}
