\version "1.0.7";

global = \notes {
  s1 | \mark "A";
  s1*2 | \mark "'12";
}

one = \notes\relative c{
  c'' c c c
  c c c c
  c c c c
}

two = \notes\relative c{
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
