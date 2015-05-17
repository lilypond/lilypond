\header
{
  texidoc = "Grace note runs have their own spacing variables in
  @code{Score.GraceSpacing}. So differing grace note lengths inside a
  run are spaced accordingly. "
}

\version "2.19.21"

\paper {  ragged-right = ##t }

\relative
{
  c''4
  \grace { c16  }
  c
  \grace { c16  }
  d
  \grace { c16 d e f }
  c
  \grace { c8 c16 d c8  }
  c
  \override Score.GraceSpacing.spacing-increment = #2.0

  \grace { c4 c16 d16  }
  c
} 
