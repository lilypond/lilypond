\version "2.19.16"

\header { texidoc = "
LilyPond should accept a tie between notes which are
enharmonically identical.
" }

\score
{
  {
    \time 3/4
    \repeat unfold 3 {
      cis'4~ des'
    }
  }
  \layout { ragged-right = ##t }
  %\midi {}
}
