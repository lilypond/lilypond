\version "2.21.0"

\header {
  texidoc = "Multi-measure rests do not have to begin and end simultaneously to be combined."
}

\score { <<
  \compressMMRests
  \partCombine
    \relative f' { R1*8  | R1*16     | R1*4 }
    \relative f' { R1*16     | R1*8  | R1*4 }
>> }

\score { <<
  \compressMMRests
  \partCombine
    \relative f' { R1*8  | r1^"r" | R1*15     | R1*4 }
    \relative f' { R1*16              | R1*8  | R1*4 }
>> }

\score { <<
  \compressMMRests
  \partCombine
    \relative f' { R1*16              | R1*8  | R1*4 }
    \relative f' { R1*8  | r1_"r" | R1*15     | R1*4 }
>> }
