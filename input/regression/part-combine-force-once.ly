
\header {
  texidoc ="Overrides for the part-combiner, affecting only one moment.
  The @code{partcombine...Once} override applies only to one moment, after which the
  old override -- if any -- is in effect again.
"
}

\layout { ragged-right = ##t }

\version "2.16.0"

mI = \relative c' {
	e4 e \partcombineApartOnce c c |
	\partcombineApart c \partcombineChordsOnce e e e |
	c \partcombineUnisonoOnce c c c |
	\partcombineAutomatic \partcombineSoloIOnce r2 c4 c |
	\partcombineSoloIIOnce R1 |
}
mII = \relative c' {
	c4 \partcombineApartOnce c c c |
	c c \partcombineAutomaticOnce e e |
	c c c c |
	R1 |
	r2 c4 c |
}

\score {
	\new Staff \partcombine \mI \mII
}
