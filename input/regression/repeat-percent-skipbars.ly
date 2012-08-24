\version "2.16.0"


\header {
  texidoc = "Percent repeats are not skipped, even when @code{skipBars} is set."
}

\layout { ragged-right= ##t }


\context Staff <<
  \set Score.skipBars = ##t
  {
    \repeat "percent" 2 { g2 a g a }
  }
>>


