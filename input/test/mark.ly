global = \melodic {
  s1 | \mark "A";
  s1*2 | \mark "'12";
}

one = \melodic\relative c{
  c'' c c c
  c c c c
  c c c c
}

two = \melodic\relative c{
  b' b b b
  b b b b
  b b b b
}

\score{
	< \global \one \two >
	\paper {
		\include "score-bar-numbering.ly";
	}
}
