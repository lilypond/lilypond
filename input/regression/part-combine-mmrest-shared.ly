\version "2.19.16"

\header {
  texidoc = "Multi-measure rests do not have to begin and end simultaneously to be combined."
}

\score { <<
  \compressFullBarRests
  \partcombine
    \relative f' { R1*8  | R1*16     | R1*4 }
    \relative f' { R1*16     | R1*8  | R1*4 }
>> }

\score { <<
  \compressFullBarRests
  \partcombine
    \relative f' { R1*8  | r1^"r" | R1*15     | R1*4 }
    \relative f' { R1*16              | R1*8  | R1*4 }
>> }

\score { <<
  \compressFullBarRests
  \partcombine
    \relative f' { R1*16              | R1*8  | R1*4 }
    \relative f' { R1*8  | r1_"r" | R1*15     | R1*4 }
>> }
