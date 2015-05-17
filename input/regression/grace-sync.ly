\version "2.19.21"
\header  {
  texidoc = "Grace notes in different voices/@/staves are synchronized."
}

\layout { ragged-right = ##t}


\relative <<
  \context Staff {
    c''2
    \grace  c8
    c4 c4
  }
  \new Staff {
    c2 \clef bass
    \grace {  dis8[ ( d8] \key es \major  }
    c4) c4
  }
  \new Staff { c2 c4 c4 \bar "|." }
>>



