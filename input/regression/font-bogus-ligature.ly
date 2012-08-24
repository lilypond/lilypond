\header
{
  texidoc = "TM and No should not be changed into trademark/@/number symbols.
This may happen with incorrect font versions.
"
}

\version "2.16.0"

\paper {
  ragged-right = ##T
}

\score{
  {
    c4^"November WHITMAN"
  }

  \layout {
    \context {
      \Score
      \override PaperColumn #'keep-inside-line = ##f
    }
  }
}
