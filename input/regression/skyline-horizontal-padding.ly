\version "2.17.6"

\header {
  texidoc = "
The skyline-horizontal-padding property can be set for System
in order to keep systems from being spaced too closely together.
In this example, the low notes from a system should not be
interleaved with the high notes from the next system.
"
}

\book {
  \score {
    {
      \override Staff.TimeSignature.stencil =  ##f
      \repeat unfold 3 { <c'''-1 e'''-3 g'''-5> c' <c,-1 e,-3 g,-5> c' \break}
    }
    \layout {
      indent = 0
      ragged-right = ##t
      \context {
        \Score
	\override System.skyline-horizontal-padding = #3.0
      }
    }
  }
}
