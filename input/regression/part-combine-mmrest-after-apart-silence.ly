\version "2.19.16"

\header {
  texidoc = "A multi-measure rest after apart-silence returns the state to unisilence."
}

\score { <<
  \new Staff {
    \partcombine
      \relative f' { r2 r2 | R1^"!!!" }
      \relative f' { R1*2       }
  }
  \new Staff {
    \partcombine
      \relative f' { R1*2       }
      \relative f' { r2 r2 | R1_"!!!" }
  }
>> }
