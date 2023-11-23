\version "2.25.11"

\header {
  texidoc = "A figured bass continuation line after an empty bass figure
has a correct vertical position."
}

<<
  \new Voice {
    \clef bass \time 3/4
    a,8 e'16 d' cis'8 b a g |
    f8 a16 g f8 e d f |
  }
  \figures {
    r8 \bassFigureExtendersOn <_>2 q8
    <6>8*5 q8
  }
>>
