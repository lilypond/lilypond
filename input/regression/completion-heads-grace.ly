\version "2.27.5"

\header { texidoc = "@code{Completion_heads_engraver} should not mix
up grace timing with notehead completion."
}

\layout {
  ragged-right = ##t
  indent = 160\pt
}


\score
{
  {
    c'2 2*7/8 \grace { 8[ 8] } s16 | c'4 \afterGrace d'2 e'8 f'4 |
  }
  \layout {
    \context {
      \Staff
      instrumentName = \markup \typewriter Note_heads_engraver
    }
  }
  \layout {
    \context {
      \Staff
      instrumentName = \markup \typewriter Completion_heads_engraver
    }
    \context {
      \Voice
      completionUnit = #1/2
      \remove Note_heads_engraver
      \consists Completion_heads_engraver
    }
  }
}
