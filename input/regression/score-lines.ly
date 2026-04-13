\version "2.25.35"

\header {
  texidoc = "The @code{\\score-lines} markup returns individual score
lines as stencils rather than a single stencil.  Calling a function
like @code{\\rotate} on @code{\\score-lines} rotates the lines
individually, as contrasted with rotating an entire @code{\\score}
markup."
}

\markup \fill-line {
  \null
  \column \rotate #-15 {
    \score-lines
    {
      \new Staff \with { instrumentName = \markup \typewriter
			 "\\score-lines" }
      \*16 c'4
      \layout {
	short-indent = 0
	indent = 0
	line-width = 4\cm
      }
    }
  }
  \column \rotate #-15 {
    \score
    {
      \new Staff \with { instrumentName = \markup \typewriter
			 "\\score" }
      \*16 c'4
      \layout {
	short-indent = 0
	indent = 0
	line-width = 4\cm
      }
    }
  }
  \null
}
